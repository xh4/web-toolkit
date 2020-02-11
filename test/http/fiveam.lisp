(in-package :http-test)

(defvar *it* nil)
(defvar *is* nil)

(defmacro => (n form)
  `(let ((*is* ',(append `(=> ,n) (list form))))
     ,form))

(defmacro it (&body body)
  (setf body
        (let ((n 1))
          (map-tree
           (lambda (form)
             (if (and (listp form)
                      (not (emptyp form))
                      (member (first form) '(fiveam:is fiveam:is-true
                                             fiveam:is-every fiveam:is-false
                                             fiveam:signals fiveam:finishes
                                             fiveam:pass fiveam:fail)))
                 (throw :skip (prog1
                                  (append `(=> ,n) (list form))
                                (incf n)))
                 form))
           body
           :tag :skip)))
  `(let ((*it* ',body))
     ,@body))

(in-package :fiveam)
(defun process-failure (test-expr &optional reason-format &rest format-args)
  (let ((reason (and reason-format
                     (apply #'format nil reason-format format-args)))
        (premble))
    (setf *it*
          (http-test::map-tree
           (lambda (form)
             (if (and (listp form)
                      (not (emptyp form))
                      (equal (first form) '=>))
                 (throw :skip
                   (if (equal (second *is*) (second form))
                       (cons '=> (cddr form))
                       (cddr form)))
                 form))
           *it*
           :tag :skip))
    (loop for form in *it*
       do (setf premble (concatenate 'string
                                     premble
                                     (format nil "~%IN ~%~A~%"
                                             (cl:with-output-to-string (stream)
                                               (pprint form stream))))))
    (setf reason (concatenate 'string premble reason))
    (with-simple-restart (ignore-failure "Continue the test run.")
      (error 'check-failure :test-expr test-expr
             :reason reason))
    (add-result 'test-failure :test-expr test-expr
                :reason reason)))
(in-package :http-test)
