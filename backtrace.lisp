(in-package :wt)

(sb-ext:with-unlocked-packages (sb-debug)
  (defun sb-debug::trace-print-unadorned (frame forms &rest args)
    (dolist (ele forms)
      (let ((values (multiple-value-list (apply (cdr ele) frame args))))
        (when (and values (car values)) ;; <- fix
          (format t "~&~{~A~^, ~}~%" values))))))

(defparameter *trace-stack* nil)

(defmacro trace-1* (name)
  (flet ((make-trace-form (name)
           `(trace ,name
                   :report nil
                   :print (progn (push (list :enter ',name sb-debug::args) *trace-stack*) nil)
                   :print-after (progn (push (list :exit ',name sb-debug::args) *trace-stack*) nil))))
    (cond
      ((stringp name)
       (let ((package (sb-impl::find-undeleted-package-or-lose name))
             (forms nil))
         (do-all-symbols (symbol (find-package name))
           (when (eql package (symbol-package symbol))
             (when (and (fboundp symbol)
                        (not (macro-function symbol))
                        (not (special-operator-p symbol)))
               (appendf forms `(,(make-trace-form symbol))))
             (let ((setf-name `(setf ,symbol)))
               (when (fboundp setf-name)
                 (appendf forms `(,(make-trace-form setf-name)))))))
         `(progn
            ,@forms)))
      ((symbolp name)
       (make-trace-form name)))))

(defmacro backtrace (&key names)
  "Middleware for outputting backtraces when an error occured."
  `(lambda (handler)
     ,@(loop for name in names
          collect `(trace-1* ,name))
     (lambda (request)
       (let ((*trace-stack* nil))
         (handler-case
             (prog1
                 (funcall handler request)
               (setf *trace-stack* nil))
           (error (e)
             `(500
               (:content-type "text/html")
               (,(make-backtrace-report request e)))))))))

(defun make-backtrace-report (request condition)
  (let* ((html `((:!doctype)
                 (:html
                        (:head
                         (:meta :charset "utf-8")
                         (:title ,(format nil "~A" condition)))
                        (:body
                         (:pre ,(format nil "~A" condition))
                         (:pre ,(format nil "~A" *trace-stack*))
                         (:pre ,(format nil "~A" (with-output-to-string (s)
                                                    (print-request s request))))
                         )))))
    (html-string html)))

(defun print-request (stream request)
  (format stream "~2&Request:~%")
  (loop for (k v) on request by #'cddr
     if (hash-table-p v) do
       (format stream "~&    ~A:~%" k)
       (maphash (lambda (k v)
                  (format stream "~&        ~A: ~S~%"
                          k v))
                v)
     else do
       (format stream
               "~&    ~A: ~S~%"
               k v)))
