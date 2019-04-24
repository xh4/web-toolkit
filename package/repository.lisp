(in-package :package)

(defclass repository () ())

(defclass git-repository (repository)
  ((url
    :initarg :url
    :initform nil
    :reader repository-url)))

(defmethod print-object ((repository git-repository) stream)
  (print-unreadable-object (repository stream :type t)
    (format stream "~A~%" (repository-url repository))))
