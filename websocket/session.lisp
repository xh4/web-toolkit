(in-package :websocket)

(defvar *session* nil)

(defclass session ()
  ((connection
    :initarg :connection
    :reader session-connection)
   (opening-uri
    :initarg :opening-uri
    :reader session-opening-uri)
   (opening-header
    :initarg :opening-header
    :reader session-opening-header)))

(defgeneric close-session (session &optional reason)
  (:method ((session session) &optional reason)
    (with-slots (connection) session
      (close-connection connection reason))))

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

(defgeneric on-message (session message))

(defun session-open-p (session)
  (let ((connection (session-connection session)))
    ;; fixme: Connection 的 state 有问题
    (let ((output-stream (slot-value connection 'output-stream)))
      (open-stream-p output-stream))))

(defmacro define-session (name superclasses slots &rest options)
  (let ((superclasses (if (find 'session superclasses)
                          superclasses
                          (append superclasses (list 'session)))))
    `(defclass ,name ,superclasses
       ,slots
       ,@options)))

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
