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
    (setf (slot-value class 'open-handler) (eval (first on-open))
          (slot-value class 'open-handler-code) (rest (first on-open))))
  (when on-close
    (setf (slot-value class 'close-handler) (eval (first on-close))
          (slot-value class 'close-handler-code) (rest (first on-close))))
  (when on-error
    (setf (slot-value class 'error-handler) (eval (first on-error))
          (slot-value class 'error-handler-code) (rest (first on-error)))))

(defmethod shared-initialize :after ((class endpoint-class) slot-names
                                      &key session-class
                                        &allow-other-keys)
  (declare (ignore slot-names))
  (when session-class
    (setf (slot-value class 'session-class) (eval (first session-class)))))

(defmacro replace-class-option (name key &rest values)
  (with-gensyms (pos)
    `(if-let ((,pos (position ,key ,name :key 'first)))
       (setf (nth ,pos ,name) (list ,key ,@values))
       (appendf ,name (list (list ,key ,@values))))))

(defmacro define-endpoint (endpoint-name superclasses slots &rest options)
  (let ((superclasses (if (find 'endpoint superclasses)
                          superclasses
                          (append superclasses (list 'endpoint)))))
    (let ((on-open (rest (find :on-open options :key 'car)))
          (on-close (rest (find :on-close options :key 'car)))
          (on-error (rest (find :on-error options :key 'car))))
      (let ((options (append options
                             `((:metaclass endpoint-class)))))
        (replace-class-option options :on-open
                              `(make-handler ,(first on-open) ,@(rest on-open)))
        (replace-class-option options :on-close
                              `(make-handler ,(first on-close) ,@(rest on-close)))
        (replace-class-option options :on-error
                              `(make-handler ,(first on-error) ,@(rest on-error)))
        (with-gensyms (s/endpoint s/request)
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (defclass ,endpoint-name ,superclasses ,slots ,@options)
               (defvar ,endpoint-name (make-instance ',endpoint-name)))
             (defmethod http:handle ((,s/endpoint ,endpoint-name) (,s/request request))
               (handle-user-endpoint-request ,s/endpoint ,s/request))
             (eval-when (:execute)
               ,endpoint-name)))))))

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
