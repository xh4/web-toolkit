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
                                     (string-to-octets reason)))))
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

(defun handle-last-frame (connection frame)
  (if (close-frame-p frame)
      (drop-connection connection)
      (drop-connection-with-error connection 1002 "Expected a close frame")))

(defun handle-fin-frame (connection frame)
  (with-slots (pending-fragments pending-opcode) connection
    (if (continuation-frame-p frame)
        (handle-continuation-frame connection frame)
        (progn
          (when pending-fragments
            (close-connection-with-error connection 1002 "All data frames after the initial data frame must have opcode 0"))
          (push frame pending-fragments)
          (setf pending-opcode (frame-opcode frame))))
    (handle-fragmentation connection)))

(defun handle-fragmentation (connection)
  (with-slots (pending-fragments pending-opcode) connection
    (let ((ordered-frames (reverse pending-fragments)))
      (cond ((eq +opcode-text+ pending-opcode)
             (let ((octets nil))
               (if (= (length ordered-frames) 1)
                   (setf octets (frame-payload-data (first ordered-frames)))
                   (let ((total-length (loop for frame in ordered-frames
                                          for data = (frame-payload-data frame)
                                          sum (length data))))
                     (setf octets (make-array total-length
                                              :element-type '(unsigned-byte 8)))
                     (loop with index = 0
                        for frame = (pop ordered-frames)
                        for data = (when frame
                                     (frame-payload-data frame))
                        while frame
                        do (loop for i across data
                              do (setf (aref octets index) i)
                                (incf index)))))
               (let ((text (octets-to-string octets :encoding :utf-8)))
                 (signal 'text-received :text text))))
            ((eq +opcode-binary+ pending-opcode)
             ;; A binary message was received
             (let ((temp-file
                    (fad:with-output-to-temporary-file
                        (fstream :element-type '(unsigned-byte 8))
                      (loop for frame in ordered-frames
                         do (write-sequence (frame-payload-data frame)
                                            fstream)))))
               (unwind-protect
                    (signal 'binary-received
                            :data temp-file)
                 (delete-file temp-file))))))
    (setf pending-fragments nil
          pending-opcode nil)))

(defun handle-non-fin-frame (connection frame)
  (with-slots (pending-fragments pending-opcode) connection
    (if (continuation-frame-p frame)
        (handle-continuation-frame connection frame)
        (progn
          (push frame pending-fragments)
          (setq pending-opcode (frame-opcode frame))))))

(defun handle-continuation-frame (connection frame)
  (with-slots (pending-fragments) connection
    (unless pending-fragments
      (close-connection-with-error connection 1002 "Unexpected continuation frame"))
    (push frame pending-fragments)))

(defun handle-control-frame (connection frame)
  (unless (frame-fin-p frame)
    (close-connection-with-error connection 1002 "Control frame must fin"))
  (when (reserved-control-frame-p frame)
    (close-connection-with-error connection 1002 "Unexpected reserved control frame"))
  (with-slots (input-stream) connection
    (cond
      ;; Ping
      ((ping-frame-p frame)
       (read-payload-data input-stream frame)
       (send-frame connection +opcode-pong+ (frame-payload-data frame)))
      ;; Pong
      ((pong-frame-p frame)
       (read-payload-data input-stream frame))
      ;; Close
      ((close-frame-p frame) (handle-close-frame connection frame)))))

(defun handle-close-frame (connection frame)
  (with-slots (input-stream) connection
    (read-payload-data input-stream frame)
    (let ((body (frame-payload-data frame)))
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
                                          (octets-to-string reason-bytes)
                                        (error (e)
                                          (declare (ignore e))
                                          (close-connection-with-error
                                           connection
                                           1002 "Malformed close reason")))))
                   (setf reason reason-string))))
             (close-connection connection :code code :reason reason)
             (drop-connection connection)))))))

(defun handle-data-frame (connection frame)
  (when (reserved-data-frame-p frame)
    (close-connection-with-error connection 1002 "Unexpected opcode"))
  (with-slots (input-stream) connection
    (read-payload-data input-stream frame))
  (if (frame-fin-p frame)
      (handle-fin-frame connection frame)
      (handle-non-fin-frame connection frame)))

(defun handle-frame (connection frame)
  (with-slots (state) connection
    (if (eq :awaiting-close state)
        (handle-last-frame connection frame)
        (if (control-frame-p frame)
            (handle-control-frame connection frame)
            (handle-data-frame connection frame)))))
