(in-package :utility)

(defun function-lambda-list (function)
  (macrolet ((with-unknown-on-error (&body body)
               `(handler-case (progn ,@body)
                  (error () :unknown))))
    (let ((cont-function-p))
      (let ((function (etypecase function
                        ;; (cl-cont::funcallable/cc
                        ;;  (setf cont-function-p t)
                        ;;  (cl-cont::f/cc-function function))
                        ((or list symbol function) function))))
        (let ((lambda-list
               #+:lispworks
                (let ((list (lw:function-lambda-list function)))
                  (if (eq list :dont-know) :unknown list))
                #+:sbcl
                (sb-introspect:function-lambda-list function)
                #+:ccl
                (multiple-value-bind (list provided) (ccl:arglist function)
                  (if provided list :unknown))
                #+:allegro
                (with-unknown-on-error
                    (excl:arglist function))))
          (if cont-function-p (rest lambda-list) lambda-list))))))
