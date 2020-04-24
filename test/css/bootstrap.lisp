(in-package :css-test)

(in-suite :css-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-bootstrap ()
    (let* ((response (http:get "https://cdn.bootcss.com/twitter-bootstrap/4.4.1/css/bootstrap.css"))
           (octets (alexandria::read-stream-content-into-byte-vector
                    (http:response-body response))))
      (prog1
          (babel:octets-to-string octets)
        (close (http:response-body response)))))

  (defun bootstrap-rules ()
    (let ((source (get-bootstrap)))
      (parse-rules source)))

  (defun bootstrap-declarations ()
    (loop for rule in (bootstrap-rules)
          when (typep rule 'qualified-rule)
          append (parse-declarations (rule-block rule)))))

(defmacro test-bootstrap ()
  `(progn
     ,@(loop for declaration in (bootstrap-declarations)
             for name = (declaration-name declaration)
             for index from 1
             for test-name = (intern (format nil "BOOTSTRAP-~A" index))
             collect `(test ,test-name
                        (it
                          (let* ((symbol (multiple-value-bind (symbol scope)
                                             (find-symbol (string-upcase ,name) :css)
                                           (if (eq :internal scope)
                                               (error "Declaration ~S not external" ,name)
                                             symbol)))
                                 (function (when symbol
                                             (handler-case
                                                 (symbol-function symbol)
                                               (error (e)
                                                 (declare (ignore e))
                                                 (error "Declaration ~S not implemented" ,name))))))
                            (unless function
                              (error "Declaration ~S not implemented" ,name))))))))

;; (test-bootstrap)
