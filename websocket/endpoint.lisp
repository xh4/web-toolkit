(in-package :websocket)

(defclass endpoint-class (standard-class)
  ((session-class
    :initarg :session-class
    :initform nil
    :accessor endpoint-session-class)
   (open-handler
    :initarg :open-handler
    :initform nil
    :accessor endpoint-open-handler)
   (open-handler-lambda-list
    :initarg :open-handler-lambda-list
    :initform nil
    :accessor endpoint-open-handler-lambda-list)
   (close-handler
    :initarg :close-handler
    :initform nil
    :accessor endpoint-close-handler)
   (close-handler-lambda-list
    :initarg :close-handler-lambda-list
    :initform nil
    :accessor endpoint-close-handler-lambda-list)
   (error-handler
    :initarg :error-handler
    :initform nil
    :accessor endpoint-error-handler)
   (error-handler-lambda-list
    :initarg :error-handler-lambda-list
    :initform nil
    :accessor endpoint-error-handler-lambda-list)))

(defmethod validate-superclass ((class endpoint-class) (super-class standard-class))
  t)

(defclass endpoint ()
  ()
  (:metaclass endpoint-class))

(defmethod shared-initialize :before ((class endpoint-class) slot-names
                                      &key on-open on-close on-error
                                        &allow-other-keys)
  (declare (ignore slot-names))
  (with-slots (open-handler open-handler-lambda-list
               close-handler close-handler-lambda-list
               error-handler error-handler-lambda-list) class
    (when on-open
      (setf open-handler (eval (car on-open))
            open-handler-lambda-list (function-lambda-list open-handler))
      (check-open-handler-lambda-list open-handler-lambda-list))
    (when on-close
      (setf close-handler (eval (car on-close))
            close-handler-lambda-list (function-lambda-list close-handler))
      (check-close-handler-lambda-list close-handler-lambda-list))
    (when on-error
      (setf error-handler (eval (car on-error))
            error-handler-lambda-list (function-lambda-list error-handler))
      (check-error-handler-lambda-list error-handler-lambda-list))))

(defmethod shared-initialize :after ((class endpoint-class) slot-names
                                      &key session-class
                                        &allow-other-keys)
  (declare (ignore slot-names))
  (when session-class
    (setf (slot-value class 'session-class) (eval (first session-class)))))

(defmacro define-endpoint (endpoint-name superclasses slots &rest options)
  (let ((superclasses (if (find 'endpoint superclasses)
                          superclasses
                          (append superclasses (list 'endpoint)))))
    (rewrite-class-option options :metaclass endpoint-class)
    (with-gensyms (endpoint/s request/s)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defclass ,endpoint-name ,superclasses ,slots ,@options)
           (defvar ,endpoint-name (make-instance ',endpoint-name)))
         (defmethod http:handle ((,endpoint/s ,endpoint-name) (,request/s request))
           (handle-user-endpoint-request ,endpoint/s ,request/s))
         (eval-when (:execute)
           ,endpoint-name)))))

(defmethod endpoint-session-class ((endpoint endpoint))
  (endpoint-session-class (class-of endpoint)))

(defmethod endpoint-session-class (endpoint)
  'session)

(defmethod endpoint-open-handler ((endpoint endpoint))
  (endpoint-open-handler (class-of endpoint)))

(defmethod endpoint-open-handler (endpoint))

(defmethod endpoint-open-handler-lambda-list ((endpoint endpoint))
  (endpoint-open-handler-lambda-list (class-of endpoint)))

(defmethod endpoint-open-handler--lambda-list (endpoint))

(defmethod endpoint-close-handler ((endpoint endpoint))
  (endpoint-close-handler (class-of endpoint)))

(defmethod endpoint-close-handler (endpoint))

(defmethod endpoint-close-handler-lambda-list ((endpoint endpoint))
  (endpoint-close-handler-lambda-list (class-of endpoint)))

(defmethod endpoint-close-handler-lambda-list (endpoint))

(defmethod endpoint-error-handler ((endpoint endpoint))
  (endpoint-error-handler (class-of endpoint)))

(defmethod endpoint-error-handler (endpoint))

(defmethod endpoint-error-handler-lambda-list ((endpoint endpoint))
  (endpoint-error-handler-lambda-list (class-of endpoint)))

(defmethod endpoint-error-handler-lambda-list (endpoint))
