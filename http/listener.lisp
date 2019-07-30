(in-package :http)

(defclass acceptor (hunchentoot:acceptor)
  ((server
    :initarg :server
    :initform nil
    :accessor acceptor-server)))

(defmethod hunchentoot:handle-request ((acceptor acceptor) request)
  (let ((server (acceptor-server acceptor)))
    (let ((handler (server-handler server)))
      ))
  (format t "~A~%" request)
  (setf (hunchentoot:content-type*) "text/html")
  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
  (let ((stream (hunchentoot:send-headers)))
    (format stream "Hello, world!")))

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
    ))

(defmethod initialize-instance :after ((listener listener) &key)
  (with-slots (port address acceptor server) listener
    (unless port
      (error "Missing port"))
    (setf acceptor (make-instance 'acceptor
                                  :port port
                                  :address address
                                  :server server))
    listener))

(defmacro define-listener (name &key port address)
  `(if (boundp ',name)
       (setf (listener-port ,name) ,port
             (listener-address ,name) ,address)
       (defvar ,name
         (make-instance 'listener
                        :port ,port
                        :address ,address))))

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
