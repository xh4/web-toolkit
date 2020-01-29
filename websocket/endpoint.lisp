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
   (open-handler-code
    :initarg :open-handler-code
    :initform nil
    :accessor endpoint-open-handler-code)
   (close-handler
    :initarg :close-handler
    :initform nil
    :accessor endpoint-close-handler)
   (close-handler-code
    :initarg :close-handler-code
    :initform nil
    :accessor endpoint-close-handler-code)
   (error-handler
    :initarg :error-handler
    :initform nil
    :accessor endpoint-error-handler)
   (error-handler-code
    :initarg :error-handler-code
    :initform nil
    :accessor endpoint-error-handler-code)))

(defclass endpoint ()
  ()
  (:metaclass endpoint-class))

(defmethod shared-initialize :before ((class endpoint-class) slot-names
                                      &key on-open on-close on-error
                                        &allow-other-keys)
  (declare (ignore slot-names))
  (when on-open
    (check-open-handler-lambda-list (car (cdadar on-open)))
    (setf (slot-value class 'open-handler) (eval (car on-open))
          (slot-value class 'open-handler-code) (cdadar on-open)))
  (when on-close
    (check-close-handler-lambda-list (car (cdadar on-close)))
    (setf (slot-value class 'close-handler) (eval (car on-close))
          (slot-value class 'close-handler-code) (cdadar on-close)))
  (when on-error
    (check-error-handler-lambda-list (car (cdadar on-error)))
    (setf (slot-value class 'error-handler) (eval (car on-error))
          (slot-value class 'error-handler-code) (cdadar on-error))))

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
    (let ((open-handler-form (second (find :on-open options :key 'car)))
          (close-handler-form (second (find :on-close options :key 'car)))
          (error-handler-form (second (find :on-error options :key 'car))))
      (rewrite-class-option options :metaclass endpoint-class)
      (replace-class-option options :on-open
                            `(make-handler ,open-handler-form))
      (replace-class-option options :on-close
                            `(make-handler ,close-handler-form))
      (replace-class-option options :on-error
                            `(make-handler ,error-handler-form))
      (with-gensyms (endpoint/s request/s)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (defclass ,endpoint-name ,superclasses ,slots ,@options)
             (defvar ,endpoint-name (make-instance ',endpoint-name)))
           (defmethod http:handle ((,endpoint/s ,endpoint-name) (,request/s request))
             (handle-user-endpoint-request ,endpoint/s ,request/s))
           (eval-when (:execute)
             ,endpoint-name))))))

(defmethod endpoint-session-class ((endpoint endpoint))
  (endpoint-session-class (class-of endpoint)))

(defmethod endpoint-session-class (endpoint)
  'session)

(defmethod endpoint-open-handler ((endpoint endpoint))
  (endpoint-open-handler (class-of endpoint)))

(defmethod endpoint-open-handler (endpoint))

(defmethod endpoint-open-handler-code ((endpoint endpoint))
  (endpoint-open-handler-code (class-of endpoint)))

(defmethod endpoint-open-handler-code (endpoint))

(defmethod endpoint-close-handler ((endpoint endpoint))
  (endpoint-close-handler (class-of endpoint)))

(defmethod endpoint-close-handler (endpoint))

(defmethod endpoint-close-handler-code ((endpoint endpoint))
  (endpoint-close-handler-code (class-of endpoint)))

(defmethod endpoint-close-handler-code (endpoint))

(defmethod endpoint-error-handler ((endpoint endpoint))
  (endpoint-error-handler (class-of endpoint)))

(defmethod endpoint-error-handler (endpoint))

(defmethod endpoint-error-handler-code ((endpoint endpoint))
  (endpoint-error-handler-code (class-of endpoint)))

(defmethod endpoint-error-handler-code (endpoint))
