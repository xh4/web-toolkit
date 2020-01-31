(in-package :websocket)

(defclass connection ()
  ((input-stream
    :initarg :input-stream
    :initform (error "Must make connection with input stream"))
   (output-stream
    :initarg :output-stream
    :initform (error "Must make connection with output streams"))
   ;; (request    :initarg :request
   ;;             :reader connection-request
   ;;             :initform (error "Must make clients with requests"))
   (write-lock
    :initform (bt:make-lock))
   (state
    :initarg :state
    :initform :disconnected) ;; :disconnected, connecting, open, :awaiting-close, :closing, closed
   (pending-fragments
    :initform nil)
   (pending-opcode
    :initform nil)))

(defun send-frame (connection opcode &optional data)
  (with-slots (state write-lock output-stream) connection
    (when (and (eq state :open)
               (open-stream-p output-stream))
      (bt:with-lock-held (write-lock)
        (write-frame output-stream opcode data)))))

(defun receive-frame (connection)
  (with-slots (input-stream) connection
    (when (open-stream-p input-stream)
      (read-frame input-stream))))

(defun close-connection (connection &key (data nil data-supplied-p)
                                      (code 1000)
                                      (reason "Normal close"))
  ;; (format t "Close connection with code: ~A, reason: ~A~%" code reason)
  (with-slots (state output-stream) connection
    (when (and (eq state :open)
               (open-stream-p output-stream))
      (send-frame connection
                  +opcode-close+
                  (if data-supplied-p
                      data
                      (concatenate 'vector
                                   (coerce (list (logand (ash code -8) #xff)
                                                 (logand code #xff))
                                           'vector)
                                   (when reason
                                     (babel:string-to-octets reason)))))
      (setf state :awaiting-close)
      (signal 'close-received :code code :reason reason))))

(defun drop-connection (connection)
  (with-slots (state output-stream) connection
    (close output-stream)
    (setf state :closed)))

(defun close-connection-with-error (connection code reason)
  (websocket-error code reason)
  (close-connection connection :code code :reason reason))

(defun drop-connection-with-error (connection code reason)
  (websocket-error code reason)
  (drop-connection connection))

(defun handle-frame (connection frame)
  (with-slots (state pending-fragments pending-opcode input-stream) connection
    (with-slots (opcode) frame
      (cond
        ((eq :awaiting-close state)
         ;; We're waiting a close because we explicitly sent one to the
         ;; connection. Error out if the next message is not a close.
         (if (close-frame-p frame)
             (drop-connection connection)
             (drop-connection-with-error connection 1002 "Expected a close frame")))
        ((not (frame-fin-p frame))
         ;; This is a non-FIN fragment Check opcode, append to connection's
         ;; fragments.
         (cond ((and (continuation-frame-p frame)
                     (not pending-fragments))
                (close-connection-with-error connection 1002 "Unexpected continuation frame"))
               ((control-frame-p frame)
                (close-connection-with-error connection 1002 "Control frames can't be fragmented"))
               ((and pending-fragments
                     (/= opcode +opcode-continuation+))
                (close-connection-with-error connection 1002 "Not discarding initiated fragment sequence"))
               (t
                ;; A data frame, is either initiaing a new fragment sequence
                ;; or continuing one
                (read-payload-data input-stream frame)
                (cond ((continuation-frame-p frame)
                       (push frame pending-fragments))
                      (t
                       (setq pending-opcode opcode
                             pending-fragments (list frame)))))))
        ((and pending-fragments
              (not (or (control-frame-p frame)
                       (continuation-frame-p frame))))
         ;; This is a FIN fragment and (1) there are pending fragments and (2)
         ;; this isn't a control or continuation frame. Error out.
         (close-connection-with-error connection 1002 "Only control frames can interleave fragment sequences."))
        (t
         ;; This is a final, FIN fragment. So first read the fragment's data
         ;; into the `data' slot.
         (cond
           ((reserved-non-control-frame-p frame)
            (close-connection-with-error connection 1002 "Unexpected opcode"))
           ((non-control-frame-p frame)
            ;; This is either a single-fragment data frame or a continuation
            ;; frame. Join the fragments and keep on processing. Join any
            ;; outstanding fragments and process the message.
            (read-payload-data input-stream frame)
            (unless pending-opcode
              (setq pending-opcode opcode))
            (let ((ordered-frames
                   (reverse (cons frame pending-fragments))))
              (cond ((eq +opcode-text+ pending-opcode)
                     ;; A text message was received
                     (signal 'text-received
                             :text (flexi-streams:octets-to-string
                                    (apply #'concatenate 'vector
                                           (mapcar #'frame-data
                                                   ordered-frames))
                                    :external-format :utf-8)))
                    ((eq +opcode-binary+ pending-opcode)
                     ;; A binary message was received
                     (let ((temp-file
                            (fad:with-output-to-temporary-file
                                (fstream :element-type '(unsigned-byte 8))
                              (loop for frame in ordered-frames
                                 do (write-sequence (frame-data frame)
                                                    fstream)))))
                       (unwind-protect
                            (signal 'binary-received
                                    :data temp-file)
                         (delete-file temp-file))))
                    (t
                     (close-connection-with-error connection 1002 "Unknown opcode"))))
            (setf pending-fragments nil))
           ((eq +opcode-ping+ opcode)
            ;; Reply to ping with a pong with the same data
            (send-frame connection +opcode-pong+ (frame-data frame)))
           ((eq +opcode-close+ opcode)
            ;; Reply to close with a close with the same data
            (let ((body (frame-data frame)))
              (cond
                ((= (length body) 0)
                 (close-connection connection)
                 (drop-connection connection))
                ((< (length body) 2) (websocket-error
                                      1002 "Malformed close body"))
                (t (let ((code (+ (* 256 (aref body 0)) (aref body 1)))
                         (reason))
                     (when (or (<= 0 code 999)
                               (<= 1004 code 1006)
                               (<= 1016 code 2999))
                       (close-connection-with-error connection 1002 "Invalid close code"))
                     (let ((reason-bytes (subseq body 2)))
                       (when (> (length reason-bytes) 0)
                         (let ((reason-string (handler-case
                                                  (babel:octets-to-string reason-bytes)
                                                (error (e)
                                                  (declare (ignore e))
                                                  (close-connection-with-error
                                                   connection
                                                   1002 "Malformed close reason")))))
                           (setf reason reason-string))))
                     (close-connection connection :code code :reason reason)
                     (drop-connection connection))))))
           ((eq +opcode-pong+ opcode))
           (t (close-connection-with-error 1002 "Unknown opcode ~a" opcode))))))))
