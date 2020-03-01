(in-package :websocket)

(defclass endpoint-class (http::handler-class)
  ((session-class
    :initarg :session-class
    :initform nil
    :accessor endpoint-session-class)
   (open-handler
    :initarg :open-handler
    :initform nil
    :accessor open-handler)
   (open-handler-lambda-list
    :initarg :open-handler-lambda-list
    :initform nil
    :accessor open-handler-lambda-list)
   (close-handler
    :initarg :close-handler
    :initform nil
    :accessor close-handler)
   (close-handler-lambda-list
    :initarg :close-handler-lambda-list
    :initform nil
    :accessor close-handler-lambda-list)
   (error-handler
    :initarg :error-handler
    :initform nil
    :accessor error-handler)
   (error-handler-lambda-list
    :initarg :error-handler-lambda-list
    :initform nil
    :accessor error-handler-lambda-list)
   (message-handler
    :initarg :message-handler
    :initform nil
    :accessor message-handler)
   (message-handler-lambda-list
    :initarg :message-handler-lambda-list
    :initform nil
    :accessor message-handler-lambda-list)))

(defmethod validate-superclass ((class endpoint-class) (super-class standard-class))
  t)

(defclass endpoint ()
  ()
  (:metaclass endpoint-class))

;; TODO: signal error when unknown class options passed
(defmethod shared-initialize :after ((class endpoint-class) slot-names
                                      &key on-open on-close on-error on-message session-class &allow-other-keys)
  (declare (ignore slot-names))
  (when on-open
    (let* ((handler (eval (car on-open)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-open-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'open-handler) handler
            (slot-value class 'open-handler-lambda-list) handler-lambda-list)))
  (when on-close
    (let* ((handler (eval (car on-close)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-close-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'close-handler) handler
            (slot-value class 'close-handler-lambda-list) handler-lambda-list)))
  (when on-error
    (let* ((handler (eval (car on-error)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-error-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'error-handler) handler
            (slot-value class 'error-handler-lambda-list) handler-lambda-list)))
  (when on-message
    (let* ((handler (eval (car on-message)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-message-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'message-handler) handler
            (slot-value class 'message-handler-lambda-list) handler-lambda-list)))
  (when session-class
    (setf (slot-value class 'session-class) (eval (car session-class)))))

(defmethod http::handler-class-precedence-list ((endpoint-class endpoint-class))
  (list endpoint-class))

(defmethod http::handler-class-precedence-list ((endpoint endpoint))
  (list (class-of endpoint)))

(defmacro define-endpoint (endpoint-name superclasses slots &rest options)
  (let ((superclasses (if (find 'endpoint superclasses)
                          superclasses
                          (append superclasses (list 'endpoint)))))
    (rewrite-class-option options :metaclass endpoint-class)
    (rewrite-class-option options :function (lambda (endpoint request)
                                              (handle-endpoint-request endpoint request)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (define-handler ,endpoint-name ,superclasses ,slots ,@options)
         (defvar ,endpoint-name (make-instance ',endpoint-name)))
       (eval-when (:execute)
         ,endpoint-name))))

(defmethod endpoint-session-class ((endpoint endpoint))
  (endpoint-session-class (class-of endpoint)))

(defmethod endpoint-session-class (endpoint)
  'session)

(defmethod open-handler ((endpoint endpoint))
  (open-handler (class-of endpoint)))

(defmethod open-handler (endpoint))

(defmethod open-handler-lambda-list ((endpoint endpoint))
  (open-handler-lambda-list (class-of endpoint)))

(defmethod open-handler-lambda-list (endpoint))

(defmethod close-handler ((endpoint endpoint))
  (close-handler (class-of endpoint)))

(defmethod close-handler (endpoint))

(defmethod close-handler-lambda-list ((endpoint endpoint))
  (close-handler-lambda-list (class-of endpoint)))

(defmethod close-handler-lambda-list (endpoint))

(defmethod error-handler ((endpoint endpoint))
  (error-handler (class-of endpoint)))

(defmethod error-handler (endpoint))

(defmethod error-handler-lambda-list ((endpoint endpoint))
  (error-handler-lambda-list (class-of endpoint)))

(defmethod error-handler-lambda-list (endpoint))

(defmethod message-handler ((endpoint endpoint))
  (message-handler (class-of endpoint)))

(defmethod message-handler (endpoint))

(defmethod message-handler-lambda-list ((endpoint endpoint))
  (message-handler-lambda-list (class-of endpoint)))

(defmethod message-handler-lambda-list (endpoint))
