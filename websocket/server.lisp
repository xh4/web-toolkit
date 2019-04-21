(in-package :websocket)

(defclass server ()
  ((port
    :initarg :port
    :initform nil
    :accessor server-port)
   (endpoints
    :initarg :endpoints
    :initform nil
    :accessor server-endpoints)
   (clack-handler
    :initform nil
    :accessor server-clack-handler)))

(defmacro define-server (name &key port endpoints)
  `(progn
     (defclass ,name (server) ())
     (unless (boundp ',name)
       (setf ,name (make-instance ',name
                                  :port ,port
                                  :endpoints ,endpoints)))))

(defgeneric start-server (server &key)
  (:method ((server server) &key)
    (with-slots (clack-handler) server
      (if clack-handler
          clack-handler
          (setf clack-handler
                (clack:clackup
                 (lambda (env)
                   (let ((ws (wsd:make-server env)))
                     (wsd:on :open ws
                             (lambda ()
                               (format t "Connected~%")))))
                 :server :hunchentoot
                 :port (server-port server)))))))

(defgeneric stop-server (server &key)
  (:method ((server server) &key)
    (when-let (clack-handler (server-clack-handler server))
      (clack:stop clack-handler)
      (setf (server-clack-handler server) nil)
      t)))

(define-server my-server :port 4000)
