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

(defvar *default-interface* "127.0.0.1")

(defun port-open-p (port &key (interface *default-interface*))
  "Determine if the port is open."
  (handler-case
      (let ((socket (usocket:socket-listen interface port
                                           :reuse-address t)))
        (usocket:socket-close socket))
    (usocket:address-in-use-error (condition)
      (declare (ignore condition))
      nil)))

(defun find-port (&key (min 40000) (max 50000) (interface *default-interface*))
  "Return the first available port in a range of port numbers."
  (loop for port from min to max until (port-open-p port :interface interface)
     finally (return port)))
