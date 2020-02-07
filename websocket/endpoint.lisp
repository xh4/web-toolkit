(in-package :websocket)

(defclass endpoint-class (http::handler-class)
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

(defmethod shared-initialize :around ((class endpoint-class) slot-names
                                      &rest args
                                      &key name direct-slots direct-superclasses location
                                        extra-initargs direct-default-initargs documentation
                                        on-open on-close on-error session-class function
                                        &allow-other-keys)
  (declare (ignore slot-names))
  ;; (format t "Shared-initialize :around (endpoint-class): ~A~%" args)
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
  (when session-class
    (setf (slot-value class 'session-class) (eval (car session-class))))
  (if (getf args :name)
      ;; First initialize
      (call-next-method class slot-names
                        :name name
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :location location
                        :function function)
      ;; Rest initialize
      (call-next-method class slot-names
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :extra-initargs extra-initargs
                        :direct-default-initargs direct-default-initargs
                        :documentation documentation
                        :location location
                        :function function)))

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
