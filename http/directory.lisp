(in-package :http)

(defclass directory ()
  ((pathname
    :initarg :pathname
    :initform nil
    :accessor directory-pathname)
   (index
    :initarg :index
    :initform nil
    :accessor directory-index)))

(defmethod print-object ((directory directory) stream)
  (print-unreadable-object (directory stream :type t :identity t)
    (format stream "~A" (pathname directory))))

(defmethod initialize-instance :after ((directory directory) &key)
  (with-slots (pathname) directory
    (check-type pathname pathname)))

(defgeneric directory-content (directory)
  (:method ((directory directory))
    ))
