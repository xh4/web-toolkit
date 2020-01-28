(in-package :websocket)

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
   (message-handler
    :initarg :message-handler
    :initform nil
    :accessor session-message-handler
    :allocation :class)
   (message-handler-code
    :initarg :message-handler-code
    :initform nil
    :accessor session-message-handler-code
    :allocation :class)
   (pool
    :initform nil
    :allocation :class
    :reader session-pool)))

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

(defgeneric on-message (session message)
  (:method (session message))
  (:method :before (session message))
  (:method :after (session message)))

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
                        (list (make-pool-slot-definition pool))
                        (make-handler-slot-definitions :message
                                                       (first on-message)
                                                       (rest on-message))))
         (options (remove-if (lambda (option)
                               (member (first option) '(:pool :on-message)))
                             options)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,session-name ,superclasses
           ,slots
           ,@options))
       (eval-when (:load-toplevel :execute)
         (let ((session (make-instance ',session-name)))
           (setf (slot-value session 'pool) ,pool
                 (slot-value session 'message-handler)
                 (make-handler ,(first on-message) ,@(rest on-message))
                 (slot-value session 'message-handler-code)
                 '(,(first on-message) ,@(rest on-message))))
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
