(in-package :http)

(defclass server ()
  ((listeners
    :initarg :listeners
    :initform nil
    :accessor server-listeners)
   (handler
    :initarg :handler
    :initform nil
    :accessor server-handler)))

(defmacro define-server (name &key listeners handler)
  `(let ((listeners ,listeners))
     (if (boundp ',name)
         (setf (server-listeners ,name) listeners
               (server-handler ,name) ,handler)
         (defvar ,name
           (make-instance 'server
                          :listeners ,listeners
                          :handler ,handler)))
     (loop for listener in listeners
        do (setf (listener-server listener) ,name))
     ,name))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t :identity t)
    ))

(defmethod initialize-instance :after ((server server) &key)
  )

(defgeneric start-server (server &key))

(defmethod start-server ((server server) &key)
  (loop for listener in (server-listeners server)
     do
       (start-listener listener)))

(defgeneric stop-server (server &key))

(defmethod stop-server ((server server) &key)
  (loop for listener in (server-listeners server)
     do
       (stop-listener listener)))
