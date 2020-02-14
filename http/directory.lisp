(in-package :http)

(defclass directory ()
  ((pathname
    :initarg :pathname
    :initform nil
    :accessor directory-pathname)))

(defmethod print-object ((directory directory) stream)
  (print-unreadable-object (directory stream :type t :identity t)
    (format stream "~S" (directory-pathname directory))))

(defmethod initialize-instance :after ((directory directory) &key)
  (with-slots (pathname) directory
    (check-type pathname pathname)
    (unless (directory-pathname-p pathname)
      (error "Pathname ~S should denote a directory" pathname))))

(defgeneric directory-content (directory)
  (:method ((directory directory))
    (loop for pathname in (cl:directory (directory-pathname directory))
       if (directory-pathname-p pathname)
       collect (make-instance 'directory :pathname pathname)
       else
       collect (make-instance 'file :pathname pathname))))

(defun directory-pathname-p (pathspec)
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and
     (not (component-present-p (pathname-name pathspec)))
     (not (component-present-p (pathname-type pathspec)))
     pathspec)))

(defgeneric directory-modify-time (directory)
  (:method ((directory directory))
    (file-write-date (directory-pathname directory))))
