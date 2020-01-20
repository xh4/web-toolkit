(in-package :websocket)

(define-handler endpoint ()
  ((session-class
    :initarg :session-class
    :initform 'session
    :accessor endpoint-session-class)))

(defmethod handle ((endpoint endpoint) (request request))
  (call-next-handler))

(defgeneric on-open (endpoint session)
  (:method (endpoint session)))

(defgeneric on-close (endpoint session code &optional reason)
  (:method (endpoint session code &optional reason)))

(defgeneric on-error (endpoint session error)
  (:method (endpoint session error)))

(defmacro define-endpoint (endpoint-name superclasses slots &rest options)
  (let ((superclasses (if (find 'endpoint superclasses)
                          superclasses
                          (append superclasses (list 'endpoint)))))
    (let ((session-class (or (second (find :session-class options :key 'car))
                             'session)))
      `(progn
         (define-handler ,endpoint-name ,superclasses ,slots)
         (defvar ,endpoint-name (make-instance ',endpoint-name
                                               :session-class ,session-class))
         (eval-when (:load-toplevel :execute)
           (setf (endpoint-session-class ,endpoint-name) ,session-class))
         (defmethod http:handle ((endpoint ,endpoint-name) (request request))
           (handle-user-endpoint-request endpoint request))
         (eval-when (:execute)
           ,endpoint-name)))))
