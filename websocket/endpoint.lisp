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

(defmacro define-endpoint (endpoint-name superclasses slots &rest options)
  (let ((superclasses (if (find 'endpoint superclasses)
                          superclasses
                          (appendf superclasses (list 'endpoint)))))
    (let ((session-class (or (second (find :session-class options :key 'car))
                             'session)))
      `(progn
         (define-handler ,endpoint-name ,superclasses ,slots)
         (if (boundp ',endpoint-name)
             (setf (endpoint-session-class ,endpoint-name) ,session-class)
             (progn
               (defvar ,endpoint-name (make-instance ',endpoint-name
                                            :session-class ,session-class))
               (defmethod http:handle ((endpoint ,endpoint-name) (request request))
                 (handle-user-endpoint-request endpoint request))))
         ,endpoint-name))))
