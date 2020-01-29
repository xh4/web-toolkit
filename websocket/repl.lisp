(in-package :websocket)

(defvar *session* nil)

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
