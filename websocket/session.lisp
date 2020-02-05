(in-package :websocket)

(defclass session-class (standard-class)
  ((message-handler
    :initarg :message-handler
    :initform nil
    :accessor session-message-handler)
   (message-handler-lambda-list
    :initarg :message-handler-lambda-list
    :initform nil
    :accessor session-message-handler-lambda-list)))

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

(defmethod shared-initialize :before ((class session-class) slot-names
                                      &key on-message
                                        &allow-other-keys)
  (declare (ignore slot-names))
  (with-slots (message-handler message-handler-lambda-list) class
    (when on-message
      (setf message-handler (eval (car on-message))
            message-handler-lambda-list (function-lambda-list message-handler))
      (check-message-handler-lambda-list message-handler-lambda-list))))

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
    (rewrite-class-option options :metaclass session-class)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,session-name ,superclasses
           ,slots
           ,@options))
       (eval-when (:load-toplevel :execute)
         (find-class ',session-name)))))
