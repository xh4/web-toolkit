(in-package :package)

(defclass package-definition (asdf:module)
  ())

(defmethod package-name ((def package-definition))
  (slot-value def 'asdf::name))

(defmethod asdf:component-name ((def package-definition))
  (let ((name (call-next-method)))
    (format nil "package/~A" name)))

(defmethod asdf:perform ((op asdf:downward-operation) (pkg package-definition))
  (let ((cmd (format nil "\"C:/Program Files/nodejs/npm.cmd\" install ~A" (package-name pkg))))
    (format t "~A~%" cmd)
    ;; (uiop:run-program cmd :output *standard-output*)
    ))

(setf (find-class 'asdf::package) (find-class 'package-definition))
