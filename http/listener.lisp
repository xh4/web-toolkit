(in-package :http)

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
   (socket
    :initarg :socket
    :initform nil
    :accessor listener-socket)
   (process
    :initarg :process
    :initform nil
    :accessor listener-process)
   (backlog
    :initarg :backlog
    :initform nil
    :accessor listener-backlog)
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
  (with-slots (port) listener
    (unless port
      (error "Missing PORT argument when initialize listener"))
    listener))

(defmacro listener (&key port address)
  `(make-instance 'listener
                  :port ,port
                  :address ,address))

(defgeneric start-listener (listener &key))

(defmethod start-listener ((listener listener) &key)
  (unless (listener-server listener)
    (error "Missing server in listener"))
  (let ((process (comm:start-up-server
                  :function (lambda (socket)
                              (make-and-process-connection listener socket))
                  :local-port (listener-port listener)
                  :local-address (listener-address listener)
                  :backlog (listener-backlog listener)
                  :announce (lambda (socket condition)
                              (setf (listener-socket listener) socket)))))
    (setf (listener-process listener) process)))

(defgeneric stop-listener (listener &key))

(defmethod stop-listener ((listener listener) &key)
  (when-let ((socket (listener-socket listener)))
    (comm::close-socket socket))
  (when-let ((process (listener-process listener)))
    (mp:process-terminate process)))

(defgeneric listener-started-p (listener))

(defmethod listener-started-p ((listener listener))
  nil)
