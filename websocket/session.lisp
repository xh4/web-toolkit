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

(defclass session ()
  ((connection
    :initarg :connection
    :initform nil
    :reader session-connection)
   (opening-uri
    :initarg :opening-uri
    :initform nil
    :reader session-opening-uri)
   (opening-header
    :initarg :opening-header
    :initform nil
    :reader session-opening-header))
  (:metaclass session-class))

(defmethod shared-initialize :around ((class session-class) slot-names
                                      &rest args
                                      &key name direct-slots direct-superclasses location
                                        extra-initargs direct-default-initargs documentation
                                        on-open on-close on-error on-message
                                        &allow-other-keys)
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
  (if (getf args :name)
      ;; First initialize
      (call-next-method class slot-names
                        :name name
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :location location)
      ;; Rest initialize
      (call-next-method class slot-names
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :extra-initargs extra-initargs
                        :direct-default-initargs direct-default-initargs
                        :documentation documentation
                        :location location)))

(defgeneric close-session  (session &optional reason)
  (:method ((session session) &optional reason)
    (with-slots (connection) session
      (close-connection connection reason))))

(defgeneric send-text (session text &key)
  (:method ((session session) text &key)
    (with-slots (connection) session
      (send-frame connection +opcode-text+
                  (string-to-octets text :encoding :utf-8)))))

(defgeneric send-binary (session data &key)
  (:method ((session session) (pathname pathname) &key)
    (with-slots (connection) session
      (let ((data (alexandria::read-file-into-byte-vector pathname)))
        (send-frame connection +opcode-binary+ data))))
  (:method ((session session) data &key)
    (with-slots (connection) session
      (send-frame connection +opcode-binary+ data))))

(defgeneric ping (session &optional data &key)
  (:method ((session session) &optional data &key)
    (with-slots (connection) session
      (send-frame connection +opcode-ping+))))

(defun session-open-p (session)
  (let ((connection (session-connection session)))
    ;; fixme: Connection 的 state 有问题
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
