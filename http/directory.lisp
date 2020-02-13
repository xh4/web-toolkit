(in-package :http)

(defclass directory ()
  ((pathname
    :initarg :pathname
    :initform nil
    :accessor pathname)
   (index
    :initarg :index
    :initform nil
    :accessor directory-index)))

(defmethod print-object ((directory directory) stream)
  (print-unreadable-object (directory stream :type t :identity t)
    (format stream "~A" (pathname directory))))
