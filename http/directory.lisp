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

(in-package :cl-user)
(defmethod pathname ((directory http::directory))
  (http::directory-pathname directory))
(in-package :http)
