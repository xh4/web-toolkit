(in-package :http)

(defclass file ()
  ((pathname
    :initarg :pathname
    :initform nil
    :accessor pathname)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type t :identity t)
    (format stream "~A" (pathname file))))
