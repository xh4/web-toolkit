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

#-lispworks
(defmethod start-listener ((listener listener) &key)
  (unless (listener-server listener)
    (error "Missing server in listener"))
  (let ((socket (usocket:socket-listen
                 (listener-address listener)
                 (listener-port listener)
                 :backlog (or (listener-backlog listener) 5)
                 :element-type '(unsigned-byte 8))))
    (setf (listener-socket listener) socket)
    (let ((process (bt:make-thread
                    (lambda ()
                      (listener-loop listener))
                    :name (format nil "Listener (~A)" (listener-port listener))
                    :initial-bindings `((*standard-output* . ,*standard-output*)
                                        (*error-output* . ,*error-output*)))))
      (setf (listener-process listener) process))))

#+lispworks
(defmethod start-listener ((listener listener) &key)
  (unless (listener-server listener)
    (error "Missing server in listener"))
  (let ((process (comm:start-up-server
                  :function (lambda (socket)
                              (make-and-process-connection listener socket))
                  :announce (lambda (socket condition)
                              (declare (ignore condition))
                              (setf (listener-socket listener) socket))
                  :backlog (or (listener-backlog listener) 5)
                  :local-port (listener-port listener)
                  :local-address (listener-address listener)
                  :process-name (format nil "Listener on port ~A" (listener-port listener)))))
    (setf (listener-process listener) process)
    listener))

#-lispworks
(defun listener-loop (listener)
  (let ((socket (listener-socket listener)))
    (loop for new-socket = (usocket:socket-accept
                            socket
                            :element-type '(unsigned-byte 8))
       do (make-and-process-connection listener new-socket))))

(defgeneric stop-listener (listener &key))

#-lispworks
(defmethod stop-listener ((listener listener) &key)
  (when-let ((socket (listener-socket listener)))
    (usocket:socket-close socket))
  (when-let ((process (listener-process listener)))
    (bt:destroy-thread process)))

#+lispworks
(defmethod stop-listener ((listener listener) &key)
  (when-let ((process (listener-process listener)))
    (comm:server-terminate process)))

(defgeneric listener-started-p (listener))

(defmethod listener-started-p ((listener listener))
  nil)
