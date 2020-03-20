(in-package :reactive)

(defgeneric react (object dependence)
  (:method (object dependency)
    (format t "React ~A ~A~%" object dependency)))
