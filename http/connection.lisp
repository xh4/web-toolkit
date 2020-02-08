(in-package :http)

(defclass connection ()
  ((listener
    :initarg :listener
    :initform nil
    :accessor connection-listener)
   (socket
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
  (let ((connection (make-connection listener socket)))
    (bt:make-thread
     (lambda ()
       (process-connection connection))
     :name (format nil "Connection on ~A" (listener-port listener))
     :initial-bindings `((*standard-output* . ,*standard-output*)
                         (*error-output* . ,*error-output*)))
    connection))

(defun make-connection (listener socket)
  (let ((stream (usocket:socket-stream socket)))
    #+lispworks
    (setf (stream:stream-read-timeout stream) 10
          (stream:stream-write-timeout stream) 10)
    (make-instance 'connection
                   :listener listener
                   :socket socket
                   :input-stream stream
                   :output-stream stream)))

(defun process-connection (connection)
  (with-slots (input-stream output-stream) connection
    (tagbody :start
       (when-let ((request (read-request input-stream)))
         (let ((response (handle-request connection request)))
           (if (equal 101 (status-code (response-status response)))
               (go :end)
               (progn
                 (handle-response connection response)
                 (if (or (not (keep-alive-p request))
                         (not (keep-alive-p response))
                         (equal 101 (status-code (response-status response)))
                         (not (open-stream-p input-stream))
                         (not (open-stream-p output-stream)))
                     (go :end)
                     (progn
                       (let ((body (request-body request)))
                         (typecase body
                           (stream (stream-read-remain body))))
                       (go :start)))))))
     :end (close-connection connection))))

(defun close-connection (connection)
  (with-slots (socket input-stream output-stream) connection
    (close input-stream)
    (close output-stream)
    (usocket:socket-close socket)))

(defun handle-request (connection request)
  (let ((listener (connection-listener connection)))
    (let ((server (listener-server listener)))
      (let ((handler (server-handler server)))
        (unless handler
          (setf handler default-handler))
        (let ((response (invoke-handler handler request)))
          (when (and (equal "HTTP/1.0" (request-version request))
                     (string-equal "Keep-Alive" (header-field-value
                                                 (find-header-field "Connection" request))))
            (set-header-field response (header-field "Connection" "Keep-Alive")))
          response)))))

(defun handle-response (connection response)
  (let ((stream (connection-output-stream connection)))
    (when (null (response-status response))
      (setf response (make-instance 'response
                                    :status 200)))
    (unless (equal 101 (status-code (response-status response)))
      (prog1
          (write-response stream response)
        (finish-output stream)))))

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
