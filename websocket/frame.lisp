(in-package :websocket)

(defstruct frame
  fin
  opcode
  masked
  payload-length
  masking-key
  payload-data)

(defun frame-fin-p (frame)
  (plusp (frame-fin frame)))

(define-constant +opcode-continuation+    #x0)
(define-constant +opcode-text+            #x1)
(define-constant +opcode-binary+          #x2)
(define-constant +opcode-close+           #x8)
(define-constant +opcode-ping+            #x9)
(define-constant +opcode-pong+            #xA)

(defun control-frame-p (frame)
  (plusp (logand #x8 (frame-opcode frame))))

(defun reserved-non-control-frame-p (frame)
  (<= 3 (frame-opcode frame) 7))

(defun reserved-data-frame-p (frame)
  (reserved-non-control-frame-p frame))

(defun reserved-control-frame-p (frame)
  (<= #xB (frame-opcode frame) #xF))

(defun non-control-frame-p (frame)
  (not (control-frame-p frame)))

(defun data-frame-p (frame)
  (non-control-frame-p frame))

(defun continuation-frame-p (frame)
  (= (frame-opcode frame) +opcode-continuation+))

(defun text-frame-p (frame)
  (= (frame-opcode frame) +opcode-text+))

(defun binary-frame-p (frame)
  (= (frame-opcode frame) +opcode-binary+))

(defun close-frame-p (frame)
  (= (frame-opcode frame) +opcode-close+))

(defun ping-frame-p (frame)
  (= (frame-opcode frame) +opcode-ping+))

(defun pong-frame-p (frame)
  (= (frame-opcode frame) +opcode-pong+))

(defun frame-masked-p (frame)
  (plusp (frame-masked frame)))

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

(defun read-frame (stream &key read-payload-p)
  (let* ((first-byte       (read-byte stream))
         (fin              (ldb (byte 1 7) first-byte))
         (extensions       (ldb (byte 3 4) first-byte))
         (opcode           (ldb (byte 4 0) first-byte))
         (second-byte      (read-byte stream))
         (masked           (ldb (byte 1 7) second-byte))
         (payload-length   (ldb (byte 7 0) second-byte))
         (payload-length   (cond ((<= 0 payload-length 125)
                                  payload-length)
                                 (t
                                  (read-unsigned-big-endian
                                   stream (case payload-length
                                            (126 2)
                                            (127 8))))))
         (masking-key      (if (plusp masked)
                               (read-n-bytes-into-sequence stream 4)))
         (extension-data   nil))
    (declare (ignore extension-data))
    (let ((frame (make-frame :fin fin
                             :opcode opcode
                             :masked masked
                             :masking-key masking-key
                             :payload-length payload-length)))
      (when (and (control-frame-p frame)
                 (> payload-length 125))
        (websocket-error
         1002 "Control frame is too large" extensions))
      (when (plusp extensions)
        (websocket-error
         1002 "No extensions negotiated, but client sends ~a!" extensions))
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

(defun read-payload-data (stream frame)
  (with-slots (masking-key payload-length payload-data) frame
    (setq payload-data (read-n-bytes-into-sequence stream
                                                   payload-length))
    (when masking-key
      (mask-unmask payload-data masking-key))))

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
