(in-package :http)

;; (setf hunchentoot:*catch-errors-p* nil)

(defclass acceptor (hunchentoot:acceptor)
  ((server
    :initarg :server
    :initform nil
    :accessor acceptor-server)))

(defmethod hunchentoot:handle-request ((acceptor acceptor) request0)
  (let ((server (acceptor-server acceptor)))
    (let ((handler (server-handler server)))
      (let ((request (make-instance 'request
                                    :method (hunchentoot:request-method request0)
                                    :uri (hunchentoot:request-uri request0))))
        (loop for (name . value) in (hunchentoot:headers-in request0)
           for field = (make-instance 'header-field
                                      :name name
                                      :value value)
           do (push field (request-header request))
           finally (reversef (request-header request)))
        (let ((response (invoke-handler handler request)))
          (handle-response response))))))

(defun handle-response (response)
  (setf (hunchentoot:return-code*) (or (response-status response) 200))
  (let ((header (response-header response)))
    (loop for field in (header-fields header)
       for name = (field-name field)
       for value = (field-value field)
       do (setf (hunchentoot:header-out name) value)))
  (let ((stream (hunchentoot:send-headers)))
    (let ((body (response-body response)))
      (typecase body
        (string
         (let ((octets (flexi-streams:string-to-octets body
                                                       :external-format :utf-8)))
           (write-sequence octets stream)))))))

(defclass listener ()
  ((port
    :initarg :port
    :initform nil
    :accessor listener-port)
   (address
    :initarg :address
    :initform nil
    :accessor listener-address)
   (name
    :initarg :name
    :initform nil
    :accessor listener-name)
   ;; 借用 Hunchentoot 的 Acceptor 实现 Listener 的功能
   (acceptor
    :initarg :acceptor
    :initform nil)
   ;; 指向所属的 Server
   (server
    :initarg :server
    :initform nil
    :accessor listener-server)))

(defmethod print-object ((listener listener) stream)
  (print-unreadable-object (listener stream :type t :identity t)
    (with-slots (port address) listener
      (let ((started-p (listener-started-p listener)))
        (format stream "Address: ~A, Port: ~A, Started: ~A" address port started-p)))))

(defmethod initialize-instance :after ((listener listener) &key)
  (with-slots (port address acceptor server) listener
    (unless port
      (error "Missing port"))
    (setf acceptor (make-instance 'acceptor
                                  :port port
                                  :address address
                                  :server server))
    listener))

(defmacro listener (&key port address)
  `(make-instance 'listener
                  :port ,port
                  :address ,address))

(defmethod (setf listener-server) (server (listener listener))
  (setf (slot-value listener 'server) server)
  (let ((acceptor (slot-value listener 'acceptor)))
    (setf (slot-value acceptor 'server) server)))

(defgeneric start-listener (listener &key))

(defmethod start-listener ((listener listener) &key)
  (unless (listener-server listener)
    (error "Missing server in listener"))
  (hunchentoot:start (slot-value listener 'acceptor)))

(defgeneric stop-listener (listener &key))

(defmethod stop-listener ((listener listener) &key)
  (hunchentoot:stop (slot-value listener 'acceptor)))

(defgeneric listener-started-p (listener))

(defmethod listener-started-p ((listener listener))
  (hunchentoot:started-p (slot-value listener 'acceptor)))
