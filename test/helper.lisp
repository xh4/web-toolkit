(in-package :test)

(defmacro ensure-cleanup (symbols &body body)
  `(unwind-protect
        (progn ,@body)
     (progn
       (mapcar 'makunbound ',symbols)
       (mapcar 'fmakunbound ',symbols))))

(defun write-forms-to-temporary-file (forms)
  (uiop:with-temporary-file (:stream stream :pathname pathname :type "lisp" :keep t)
    (format stream "(in-package ~S)" (package-name *package*))
    (let ((*print-case* :downcase))
      (loop for form in forms
         do
           (format stream "~%")
           (pprint form stream)))
    pathname))

(defmacro compile-and-load-toplevel-forms (&rest forms)
  `(let ((pathname (write-forms-to-temporary-file ',forms)))
     ;; (format t "~%~A~%" (read-file-into-string pathname))
     (load (compile-file pathname))))
