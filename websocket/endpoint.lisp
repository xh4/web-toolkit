(in-package :websocket)

(define-handler endpoint ()
  ((session-class
    :initarg :session-class
    :initform 'session
    :accessor endpoint-session-class)))

(defmethod handle ((endpoint endpoint) (request request))
  (call-next-handler))

(defgeneric on-open (endpoint session))

(defgeneric on-close (endpoint session code &optional reason))

(defgeneric on-error (endpoint session error))

(defmacro define-endpoint (name &key session-class)
  `(progn
     (define-handler ,name (endpoint) ())
     (defmethod http:handle ((endpoint ,name) (request request))
       (handle-user-endpoint-request endpoint request))
     (if (boundp ',name)
         (setf (endpoint-session-class ,name) ,session-class)
       (defvar ,name (make-instance ',name
                                    :session-class ,session-class)))
     ,name))
