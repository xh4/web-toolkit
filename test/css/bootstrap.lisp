(in-package :css-test)

(in-suite :css-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-bootstrap ()
    (let* ((response (http:get "https://cdn.bootcss.com/twitter-bootstrap/4.4.1/css/bootstrap.css"))
           (octets (alexandria::read-stream-content-into-byte-vector
                    (http:response-body response))))
      (babel:octets-to-string octets)))

  (defun bootstrap-rules ()
    (let ((code (get-bootstrap)))
      (let ((code-without-comments (with-output-to-string (output)
                                     (with-input-from-string (input code)
                                       (loop for line = (read-line input nil nil)
                                          while line
                                          unless (cl-ppcre:scan "^/|\\s+\\*" line)
                                          do (write-line line output))))))
        (let ((rules (cl-ppcre:split "\\n\\n" code-without-comments)))
          (loop for rule in rules
             unless (cl-ppcre:scan "^@" rule)
             collect rule)))))

  (defun bootstrap-declarations ()
    (loop for rule in (bootstrap-rules)
       do (setf rule (cl-ppcre:regex-replace-all "\\n" rule " "))
       append (let ((body (first
                           (coerce
                            (nth-value 1 (cl-ppcre:scan-to-strings ".*{(.*)}" rule))
                            'list))))
                (loop for dec in (cl-ppcre:split "\\s*;\\s*" body)
                   for declaration = (string-trim '(#\Space) dec)
                   for (name value) = (cl-ppcre:split "\\s*:\\s*" declaration)
                   unless (cl-ppcre:scan "^-" name)
                   collect `(,name . ,value))))))

(defmacro test-bootstrap ()
  `(progn
     ,@(loop for (name . value) in (bootstrap-declarations)
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
                                              (error "Declaration ~S not implemented" ,name))))))
                         (if function
                             (let ((property (funcall function ,value)))
                               (is (equal (format nil "~A: ~A" ,name ,value) (serialize property))))
                             (error "Declaration ~S not implemented" ,name))))))))

(test-bootstrap)