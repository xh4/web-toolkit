(in-package :websocket)

(defclass endpoint ()
  ((session-class
    :initarg :session-class
    :initform 'session
    :accessor endpoint-session-class)))

(defgeneric on-open (endpoint session)
  (:method (endpoint session) session))

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
      (with-gensyms (s/endpoint s/request)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (defclass ,endpoint-name ,superclasses ,slots)
             (defvar ,endpoint-name (make-instance ',endpoint-name
                                                   :session-class ,session-class)))
           (eval-when (:load-toplevel :execute)
             (setf (endpoint-session-class ,endpoint-name) ,session-class))
           (defmethod http:handle ((,s/endpoint ,endpoint-name) (,s/request request))
             (handle-user-endpoint-request ,s/endpoint ,s/request))
           (eval-when (:execute)
             ,endpoint-name))))))
