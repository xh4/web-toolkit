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

(defvar *session* nil)

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
    (setf (slot-value class 'message-handler) (eval (first on-message))
          (slot-value class 'message-handler-code) (rest (first on-message)))))

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

(defun make-pool-slot-definition (pool)
  `(pool
    :initform ,pool
    :allocation :class
    :reader session-pool))

(defmacro define-session (session-name superclasses slots &rest options)
  (let* ((superclasses (if (find 'session superclasses)
                           superclasses
                           (append superclasses (list 'session))))
         (pool (second (find :pool options :key 'first)))
         (on-message (rest (find :on-message options :key 'first)))
         (slots (append slots
                        (list (make-pool-slot-definition pool))))
         (options (remove-if (lambda (option)
                               (member (first option) '(:pool)))
                             (append options
                                     `((:metaclass session-class))))))
    (replace-class-option options :on-message
                          `(make-handler ,(first on-message) ,@(rest on-message)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,session-name ,superclasses
           ,slots
           ,@options))
       (eval-when (:load-toplevel :execute)
         (let ((session (make-instance ',session-name)))
           (setf (slot-value session 'pool) ,pool))
         (find-class ',session-name)))))

;; TODO: 处理 session 关闭的情况
;; TODO: 处理抛出异常的情况
(defun in-session (session)
  (when *session*
    (error "Already in session ~A" *session*))
  (unless session
    )
  (unless (typep session 'session)
    )
  (loop
     (let ((*session* session))
       (format t "~A : ~A > " (package-name *package*) session)
       (finish-output)
       (let ((input-form (read *standard-input* nil nil)))
         (let ((result (case input-form
                         (:q (return session))
                         (t (handler-case
                                (eval input-form)
                              (error (e)
                                (invoke-debugger e)))))))
           (format t "~A~%" result))))))
