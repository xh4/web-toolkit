(in-package :wt)

(defclass listener ()
  ((port
    :initarg :port
    :accessor listener-port
    :type integer)
   (socket
    :initform nil
    :accessor listener-socket)
   (acceptor
    :initform nil
    :accessor listener-acceptor)
   (kernel
    :initform nil
    :accessor listener-kernel)
   (router
    :initarg :router
    :initform nil
    :accessor listener-router
    :type router)))

(defmethod initialize-instance :after ((listener listener) &key)
  (when (not (listener-router listener))
    (error "Must specify a router for listener")))

(defgeneric handle-request (listener request)
  (:method ((listener listener) request)
    (with-slots (static-routes router) listener
      (let ((*response* (make-instance 'response)))

        (handler-case
            (let ((handler (match-route-with-request router request)))
              (if handler
                  (call-with-request (symbol-value handler) request)
                  (handle-not-found listener)))
          (error (e)
            (format t "~A~%" e)
            (handle-error listener)))

        *response*))))

(defgeneric handle-not-found (listener)
  (:method ((listener listener))
    (setf (response-code *response*) 404)
    (setf (response-headers *response*) '(("Content-Type" . "text/html")))
    (setf (response-body *response*) "<h1>404 Not Found</h1>")))

(defgeneric handle-error (listener)
  (:method ((listener listener))
    (setf (response-code *response*) 500)
    (setf (response-headers *response*) '(("Content-Type" . "text/html")))
    (setf (response-body *response*) "<h1>500 Internal Server Error</h1>")))

(defgeneric read-request (listener socket)
  (:method ((listener listener) socket)
    (let* ((stream (usocket:socket-stream socket))
           (size (read-value 'u4 stream))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      (let ((request (make-instance 'request)))
        (pb:merge-from-array request buffer 0 size)
        request))))

(defgeneric write-response (listener socket response)
  (:method ((listener listener) socket response)
    (let* ((stream (usocket:socket-stream socket))
           (size (pb:octet-size response))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (pb:serialize response buffer 0 size)
      (write-value 'u4 stream size)
      (write-sequence buffer stream)
      (force-output stream))))

(defgeneric handle-connection (listener socket)
  (:method ((listener listener) socket)
    (handler-bind ((condition #'(lambda (condition)
                                  (declare (ignore condition))
                                  (usocket:socket-close socket))))
      (let ((request (read-request listener socket)))
        (let ((response (handle-request listener request)))
          (when response
            (write-response listener socket response))))
      (usocket:socket-close socket))))

(defgeneric make-acceptor (listener)
  (:method ((listener listener))
    (with-slots (socket acceptor) listener
      (let ((thread
             (bt:make-thread
              (lambda ()
                (let ((server-socket (listener-socket listener)))
                  (task-handler-bind ((error #'invoke-transfer-error))
                    (loop do (handler-case
                                 (let* ((client-socket (usocket:socket-accept server-socket
                                                                              :element-type '(unsigned-byte 8)))
                                        (*kernel* (listener-kernel listener))
                                        (channel (make-channel)))
                                   (submit-task channel 'handle-connection listener client-socket))
                               (error (e)
                                 (format t "~A~%" e)))))))
              :initial-bindings `((*standard-output* . ,*standard-output*)
                                  (listener . ,listener)))))
        (setf acceptor thread)))))

(defun start-listener (listener)
  (with-slots (port socket acceptor kernel) listener
    (when socket
      (stop-listener listener))
    (setf socket
          (usocket:socket-listen usocket:*wildcard-host*
                                 port
                                 :reuseaddress t
                                 :element-type '(unsigned-byte 8)))
    (setf acceptor
          (make-acceptor listener))
    (setf kernel
          (make-kernel 16 :name "WT-Worker"))
    t))

(defun stop-listener (listener &key soft)
  (with-slots (socket acceptor kernel) listener
    (when socket
      (bt:destroy-thread acceptor)
      (setf acceptor nil)
      (let ((*kernel* kernel))
        (end-kernel)
        (setf kernel nil))
      (usocket:socket-close socket)
      (setf socket nil)
      t)))

(defmacro define-listener (name options &rest forms)
  `(let ((router (make-router ,@forms)))
     (if (boundp ',name)
         (progn
           (format t "Redefining ~A~%" ',name)
           (when-let ((port (getf ',options :port)))
             (setf (listener-port ,name) port))
           (setf (listener-router ,name) router))
         (defparameter ,name (make-instance 'listener ,@options
                                            :router router)))))
