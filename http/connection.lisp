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
    #+lispworks
    (setf (stream:stream-read-timeout stream) 10
          (stream:stream-write-timeout stream) 10)
    (make-instance 'connection
                   :socket socket
                   :input-stream stream
                   :output-stream stream)))

(defun process-connection (listener connection)
  (with-slots (input-stream output-stream) connection
    (tagbody :start
       (when-let ((request (read-request input-stream)))
         (let ((response (handle-request listener connection request)))
           (when (and (equal "HTTP/1.0" (request-version request))
                    (string-equal "Keep-Alive" (header-field-value
                                                (find-header-field "Connection" request))))
             (set-header-field response (header-field "Connection" "Keep-Alive")))
           (if (= 101 (status-code (response-status response)))
               (go :end)
               (progn
                 (handle-response listener connection response)
                 (if (or (not (keep-alive-p request))
                         (not (keep-alive-p response))
                         (= 101 (status-code (response-status response)))
                         (not (open-stream-p input-stream))
                         (not (open-stream-p output-stream)))
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
  (declare (ignore listener))
  (let ((stream (connection-output-stream connection)))
    (prog1
        (write-response stream response)
      (finish-output stream))))

(defgeneric keep-alive-p (object)
  (:method ((request request))
    (let ((header (request-header request)))
      (or (and (equal "HTTP/1.0" (request-version request))
               (string-equal "Keep-Alive" (header-field-value
                                           (find-header-field "Connection" header))))
          (and (equal "HTTP/1.1" (request-version request))
               (not (string-equal "Close" (header-field-value
                                           (find-header-field "Connection" header))))))))
  (:method ((response response))
    (let ((header (response-header response)))
      (not (string-equal "Close" (header-field-value
                                  (find-header-field "Connection" header))))))
  (:method ((entity entity))
    (let ((header (response-header entity)))
      (not (string-equal "Close" (header-field-value
                                  (find-header-field "Connection" header)))))))
