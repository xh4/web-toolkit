(in-package :websocket)

(define-handler endpoint ()
  ((path
    :initarg :path
    :initform nil
    :accessor endpoint-path)
   (session-class
    :initarg :session-class
    :initform 'session
    :accessor endpoint-session-class)))


(defmethod handle ((endpoint endpoint) (request request))
  (call-next-handler))


(defgeneric on-open (endpoint session))

(defgeneric on-close (endpoint &optional reason))

(defgeneric on-error (endpoint error))


(defmacro define-endpoint (name &key path session-class)
  `(progn
     (define-handler ,name (endpoint) ())
     (defmethod http:handle ((endpoint ,name) (request request))
       (handle-user-endpoint-request endpoint request))
     (if (boundp ',name)
         (setf (endpoint-path ,name) ,path
               (endpoint-session-class ,name) ,session-class)
       (defvar ,name (make-instance ',name
                                    :path ,path
                                    :session-class ,session-class)))
     ,name))
