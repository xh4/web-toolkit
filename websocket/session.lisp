(in-package :websocket)

(defclass session-class (standard-class)
  ((message-handler
    :initarg :message-handler
    :initform nil
    :accessor session-message-handler)
   (message-handler-code
    :initarg :message-handler-code
    :initform nil
    :accessor session-message-handler-code)))

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
    :reader session-opening-header
    :allocation :class)
   (pool
    :initform nil
    :allocation :class
    :reader session-pool))
  (:metaclass session-class))

(defmethod shared-initialize :before ((class session-class) slot-names
                                      &key on-message
                                        &allow-other-keys)
  (declare (ignore slot-names))
  (when on-message
    (check-message-handler-lambda-list (car (cdadar on-message)))
    (setf (slot-value class 'message-handler) (eval (car on-message))
          (slot-value class 'message-handler-code) (cdadar on-message))))

(defgeneric close-session  (session &optional reason)
  (:method ((session session) &optional reason)))

(defmethod close-session :after (session &optional reason)
  (with-slots (connection) session
    (close-connection connection reason)))

(defgeneric send-text (session text &key)
  (:method ((session session) text &key)
    (with-slots (connection) session
      (send-frame connection +text-frame+
                  (flex:string-to-octets text
                                         :external-format :utf-8)))))

(defgeneric send-binary (session data &key)
  (:method ((session session) (pathname pathname) &key)
    (with-slots (connection) session
      (let ((data (alexandria::read-file-into-byte-vector pathname)))
        (send-frame connection +binary-frame+ data))))
  (:method ((session session) data &key)
    (with-slots (connection) session
      (send-frame connection +binary-frame+ data))))

(defgeneric ping (session &optional data &key)
  (:method ((session session) &optional data &key)
    (with-slots (connection) session
      (send-frame connection +ping+))))

(defun session-open-p (session)
  (let ((connection (session-connection session)))
    ;; fixme: Connection 的 state 有问题
    (let ((output-stream (slot-value connection 'output-stream)))
      (open-stream-p output-stream))))

(defmacro define-session (session-name superclasses slots &rest options)
  (let* ((superclasses (if (find 'session superclasses)
                           superclasses
                           (append superclasses (list 'session))))
         (message-handler-form (second (find :on-message options :key 'first))))
    (rewrite-class-option options :metaclass session-class)
    (replace-class-option options :on-message
                          `(make-handler ,message-handler-form))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,session-name ,superclasses
           ,slots
           ,@options))
       (eval-when (:load-toplevel :execute)
         (find-class ',session-name)))))
