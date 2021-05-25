(in-package :websocket)

(defclass session-class (standard-class)
  ((open-handler
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

(defmethod validate-superclass ((class session-class) (super-class standard-class))
  t)

(defvar *session-connection-mapping* (make-weak-hash-table :weakness :key))

(defclass session ()
  ()
  (:metaclass session-class))

(defmethod shared-initialize :after ((class session-class) slot-names
                                      &key on-open on-close on-error on-message
                                        &allow-other-keys)
  (declare (ignore slot-names))
  (if on-open
    (let* ((handler (eval (car on-open)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-open-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'open-handler) handler
            (slot-value class 'open-handler-lambda-list) handler-lambda-list))
    (setf (slot-value class 'open-handler) nil
          (slot-value class 'open-handler-lambda-list) nil))
  (if on-close
    (let* ((handler (eval (car on-close)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-close-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'close-handler) handler
            (slot-value class 'close-handler-lambda-list) handler-lambda-list))
    (setf (slot-value class 'close-handler) nil
          (slot-value class 'close-handler-lambda-list) nil))
  (if on-error
    (let* ((handler (eval (car on-error)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-error-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'error-handler) handler
            (slot-value class 'error-handler-lambda-list) handler-lambda-list))
    (setf (slot-value class 'error-handler) nil
          (slot-value class 'error-handler-lambda-list) nil))
  (if on-message
    (let* ((handler (eval (car on-message)))
           (handler-lambda-list (function-lambda-list handler)))
      (check-message-handler-lambda-list handler-lambda-list)
      (setf (slot-value class 'message-handler) handler
            (slot-value class 'message-handler-lambda-list) handler-lambda-list))
    (setf (slot-value class 'message-handler) nil
          (slot-value class 'message-handler-lambda-list) nil)))

(defmethod http:connection ((session session))
  (gethash session *session-connection-mapping*))

(defgeneric close-session  (session &optional reason)
  (:method ((session session) &optional reason)
    (let ((connection (http:connection session)))
      (close-connection connection :reason reason))))

(defgeneric send-text (session text &key)
  (:method ((session session) text &key)
    (let ((connection (http:connection session)))
      (send-frame connection +opcode-text+
                  (string-to-octets text :encoding :utf-8)))))

(defgeneric send-binary (session data &key)
  (:method ((session session) (pathname pathname) &key)
    (let ((connection (http:connection session)))
      (let ((data (alexandria::read-file-into-byte-vector pathname)))
        (send-frame connection +opcode-binary+ data))))
  (:method ((session session) data &key)
    (let ((connection (http:connection session)))
      (send-frame connection +opcode-binary+ data))))

(defgeneric ping (session &optional data &key)
  (:method ((session session) &optional data &key)
    (let ((connection (http:connection session)))
      (send-frame connection +opcode-ping+))))

(defun session-open-p (session)
  (let ((connection (http:connection session)))
    ;; FIXME: Connection 的 State 有问题
    (let ((output-stream (slot-value connection 'output-stream)))
      (open-stream-p output-stream))))

(defmacro define-session (session-name superclasses slots &rest options)
  (let* ((superclasses (if (find 'session superclasses)
                           superclasses
                           (append superclasses (list 'session)))))
    (unless (find :metaclass options :key 'first)
      (rewrite-class-option options :metaclass session-class))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,session-name ,superclasses
           ,slots
           ,@options))
       (eval-when (:load-toplevel :execute)
         (find-class ',session-name)))))

(defmethod open-handler ((session session))
  (open-handler (class-of session)))

(defmethod open-handler (session))

(defmethod open-handler-lambda-list ((session session))
  (open-handler-lambda-list (class-of session)))

(defmethod open-handler-lambda-list (session))

(defmethod close-handler ((session session))
  (close-handler (class-of session)))

(defmethod close-handler (session))

(defmethod close-handler-lambda-list ((session session))
  (close-handler-lambda-list (class-of session)))

(defmethod close-handler-lambda-list (session))

(defmethod error-handler ((session session))
  (error-handler (class-of session)))

(defmethod error-handler (session))

(defmethod error-handler-lambda-list ((session session))
  (error-handler-lambda-list (class-of session)))

(defmethod error-handler-lambda-list (session))

(defmethod message-handler ((session session))
  (message-handler (class-of session)))

(defmethod message-handler (session))

(defmethod message-handler-lambda-list ((session session))
  (message-handler-lambda-list (class-of session)))

(defmethod message-handler-lambda-list (session))
