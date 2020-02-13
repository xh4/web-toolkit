(in-package :http)

(defclass file ()
  ((pathname
    :initarg :pathname
    :initform nil
    :accessor file-pathname)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type t :identity t)
    (format stream "~A" (pathname file))))

(in-package :cl-user)
(defmethod pathname ((file http::file))
  (http::file-pathname file))
(in-package :http)
