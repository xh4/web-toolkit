(in-package :http)

(defclass connection ()
  ((socket
    :initarg :socket
    :initform nil
    :accessor connection-socket)
   (input-stream
    :initarg :input-stream
    :initform nil
    :accessor connection-input-stream)
   (output-stream
    :initarg :output-stream
    :initform nil
    :accessor connection-output-stream)))

(defun make-and-process-connection (listener socket)
  (let ((connection (make-connection socket)))
    (bt:make-thread
     (lambda ()
       (process-connection listener connection))
     :initial-bindings `((*standard-output* . ,*standard-output*)
                         (*error-output* . ,*error-output*)))
    connection))

(defun make-connection (socket)
  (let ((stream (usocket:socket-stream socket)))
    (make-instance 'connection
                   :socket socket
                   :input-stream stream
                   :output-stream stream)))

(defun process-connection (listener connection)
  (with-slots (input-stream output-stream) connection
    (tagbody :start
       (if-let ((request (read-request input-stream)))
         (let ((response (handle-request listener connection request)))
           (if (= 101 (status-code (response-status response)))
               (go :end)
               (progn
                 (handle-response listener connection response)
                 (if (or (search "close" (header-field-value
                                          (find-header-field request "Connection")))
                         (search "close" (header-field-value
                                          (find-header-field response "Connection")))
                         (= 101 (status-code (response-status response)))
                         (not (open-stream-p input-stream)))
                     (go :end)
                     (go :start))))))
     :end (close-connection connection))))

(defun close-connection (connection)
  (with-slots (socket input-stream output-stream) connection
    (close input-stream)
    (close output-stream)
    (usocket:socket-close socket)))

(defun handle-request (listener connection request)
  (let ((server (listener-server listener)))
    (let ((handler (server-handler server)))
      (unless handler
        (setf handler default-handler))
      (invoke-handler handler request))))

(defun handle-response (listener connection response)
  (let ((stream (connection-output-stream connection)))
    (write-response stream response)
    (force-output stream)))
