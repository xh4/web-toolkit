(in-package :websocket)

(define-constant +continuation-frame+    #x0)
(define-constant +text-frame+            #x1)
(define-constant +binary-frame+          #x2)
(define-constant +connection-close+      #x8)
(define-constant +ping+                  #x9)
(define-constant +pong+                  #xA)

(defun control-frame-p (opcode)
  (plusp (logand #x8 opcode)))

(defun read-unsigned-big-endian (stream n)
  "Read N bytes from stream and return the big-endian number"
  (loop for i from (1- n) downto 0
     sum (* (read-byte stream) (expt 256 i))))

(defun read-n-bytes-into-sequence (stream n)
  "Return an array of N bytes read from stream"
  (let* ((array (make-array n :element-type '(unsigned-byte 8)))
         (read (read-sequence array stream)))
    (assert (= read n) nil
            "Expected to read ~a bytes, but read ~a" n read)
    array))

(defclass frame ()
  ((opcode          :initarg :opcode :accessor frame-opcode)
   (data                             :accessor frame-data)
   (finp            :initarg :finp)
   (payload-length  :initarg :payload-length :accessor frame-payload-length)
   (masking-key     :initarg :masking-key)))

(defun read-frame (stream &key read-payload-p)
  (let* ((first-byte       (read-byte stream))
         (fin              (ldb (byte 1 7) first-byte))
         (extensions       (ldb (byte 3 4) first-byte))
         (opcode           (ldb (byte 4 0) first-byte))
         (second-byte      (read-byte stream))
         (mask-p           (plusp (ldb(byte 1 7) second-byte)))
         (payload-length   (ldb (byte 7 0) second-byte))
         (payload-length   (cond ((<= 0 payload-length 125)
                                  payload-length)
                                 (t
                                  (read-unsigned-big-endian
                                   stream (case payload-length
                                            (126 2)
                                            (127 8))))))
         (masking-key      (if mask-p (read-n-bytes-into-sequence stream 4)))
         (extension-data   nil))
    (declare (ignore extension-data))
    (when (and (control-frame-p opcode)
               (> payload-length 125))
      (websocket-error
       1002 "Control frame is too large" extensions))
    (when (plusp extensions)
      (websocket-error
       1002 "No extensions negotiated, but client sends ~a!" extensions))
    (let ((frame
           (make-instance 'frame :opcode opcode
                          :finp (plusp fin)
                          :masking-key masking-key
                          :payload-length payload-length)))
      (when (or (control-frame-p opcode)
                read-payload-p)
        (read-payload stream frame))
      frame)))

(defun mask-unmask (data masking-key)
  ;; RFC6455 Masking
  ;;
  ;; Octet i of the transformed data
  ;; ("transformed-octet-i") is the XOR of octet i
  ;; of the original data ("original-octet-i")
  ;; with octet at index i modulo 4 of the masking
  ;; key ("masking-key-octet-j"):
  (loop for i from 0 below (length data)
     do (setf (aref data i)
              (logxor (aref data i)
                      (aref masking-key
                            (mod i 4)))))
  data)

(defun read-payload (stream frame)
  (with-slots (masking-key payload-length data) frame
    (setq data (read-n-bytes-into-sequence stream
                                           payload-length))
    (when masking-key
      (mask-unmask data masking-key))))

(defun write-frame (stream opcode &optional data)
  (let* ((first-byte     #x00)
         (second-byte    #x00)
         (len            (if data (length data) 0))
         (payload-length (cond ((<= len 125)        len)
                               ((< len (expt 2 16)) 126)
                               (t                   127)))
         (mask-p         nil))
    (setf (ldb (byte 1 7) first-byte)  1
          (ldb (byte 3 4) first-byte)  0
          (ldb (byte 4 0) first-byte)  opcode
          (ldb (byte 1 7) second-byte) (if mask-p 1 0)
          (ldb (byte 7 0) second-byte) payload-length)
    (write-byte first-byte stream)
    (write-byte second-byte stream)
    (loop for i from  (1- (cond ((= payload-length 126) 2)
                                ((= payload-length 127) 8)
                                (t                      0)))
       downto 0
       for out = (ash len (- (* 8 i)))
       do (write-byte (logand out #xff) stream))
    ;; (if mask-p
    ;;     (error "sending masked messages not implemented yet"))
    (if data (write-sequence data stream))
    (force-output stream)))

(defun check-message (opcode fragment-length total-length)
  (cond ((> fragment-length #xffff) ; 65KiB
         (websocket-error 1009 "Message fragment too big"))
        ((> total-length #xfffff) ; 1 MiB
         (websocket-error 1009 "Total message too big")))
  (when (eql opcode +binary-frame+)
    (websocket-error 1003 "Binaries not accepted")))

(defun handle-frame (connection frame)
  (with-slots (state pending-fragments pending-opcode input-stream) connection
    (with-slots (opcode finp payload-length masking-key) frame
      (flet ((maybe-accept-data-frame ()
               (check-message (or pending-opcode
                                  opcode)
                              payload-length
                              (+ payload-length
                                 (reduce #'+ (mapcar
                                              #'frame-payload-length
                                              pending-fragments))))
               (read-payload input-stream frame)))
        (cond
          ((eq :awaiting-close state)
           ;; We're waiting a close because we explicitly sent one to the
           ;; connection. Error out if the next message is not a close.
           ;;
           (unless (eq opcode +connection-close+)
             (websocket-error
              1002 "Expected connection close from connection, got 0x~x" opcode))
           (setq state :closed))
          ((not finp)
           ;; This is a non-FIN fragment Check opcode, append to connection's
           ;; fragments.
           ;;
           (cond ((and (= opcode +continuation-frame+)
                       (not pending-fragments))
                  (websocket-error
                   1002 "Unexpected continuation frame"))
                 ((control-frame-p opcode)
                  (websocket-error
                   1002 "Control frames can't be fragmented"))
                 ((and pending-fragments
                       (/= opcode +continuation-frame+))
                  (websocket-error
                   1002 "Not discarding initiated fragment sequence"))
                 (t
                  ;; A data frame, is either initiaing a new fragment sequence
                  ;; or continuing one
                  (maybe-accept-data-frame)
                  (cond ((= opcode +continuation-frame+)
                         (push frame pending-fragments))
                        (t
                         (setq pending-opcode opcode
                               pending-fragments (list frame)))))))
          ((and pending-fragments
                (not (or (control-frame-p opcode)
                         (= opcode +continuation-frame+))))
           ;; This is a FIN fragment and (1) there are pending fragments and (2)
           ;; this isn't a control or continuation frame. Error out.
           (websocket-error
            1002 "Only control frames can interleave fragment sequences."))
          (t
           ;; This is a final, FIN fragment. So first read the fragment's data
           ;; into the `data' slot.
           (cond
             ((not (control-frame-p opcode))
              ;; This is either a single-fragment data frame or a continuation
              ;; frame. Join the fragments and keep on processing. Join any
              ;; outstanding fragments and process the message.
              (maybe-accept-data-frame)
              (unless pending-opcode
                (setq pending-opcode opcode))
              (let ((ordered-frames
                     (reverse (cons frame pending-fragments))))
                (cond ((eq +text-frame+ pending-opcode)
                       ;; A text message was received
                       (signal 'text-received
                               :text (flexi-streams:octets-to-string
                                      (apply #'concatenate 'vector
                                             (mapcar #'frame-data
                                                     ordered-frames))
                                      :external-format :utf-8)))
                      ((eq +binary-frame+ pending-opcode)
                       ;; A binary message was received
                       (let ((temp-file
                              (fad:with-output-to-temporary-file
                                  (fstream :element-type '(unsigned-byte 8))
                                (loop for frame in ordered-frames
                                   do (write-sequence (frame-data frame)
                                                      fstream)))))
                         (unwind-protect
                              (signal :binary-received
                                      :data temp-file)
                              (delete-file temp-file))))
                      (t
                       (websocket-error
                        1002 "Unknown opcode ~a" opcode))))
              (setf pending-fragments nil))
             ((eq +ping+ opcode)
              ;; Reply to ping with a pong with the same data
              (send-frame connection +pong+ (frame-data frame)))
             ((eq +connection-close+ opcode)
              ;; Reply to close with a close with the same data
              (let ((reason (frame-data frame)))
                (close-connection connection :data reason)
                (signal 'close-received :reason reason))
              (setq state :closed))
             ((eq +pong+ opcode)
              ;; Probably just a heartbeat, don't do anything.
              )
             (t
              (websocket-error
               1002 "Unknown opcode ~a" opcode)))))))))
