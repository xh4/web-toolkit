(in-package :http)

(defclass directory-entity (entity)
  ((directory
    :initarg :directory
    :initform nil
    :reader entity-directory)
   (html
    :initarg :html
    :initform nil
    :reader entity-html)))

(defun make-directory-entity (pathname &key status header)
  (check-type pathname pathname)
  (unless (directory-pathname-p pathname)
    (error "Pathname ~S should denote a directory" pathname))
  (let* ((directory (make-instance 'directory :pathname pathname))
         (last-modified (rfc-1123-date (directory-modify-time directory)))
         (status (or status
                     (if (probe-file pathname)
                         200
                         404)))
         (html (directory-html directory))
         (body (babel:string-to-octets (html:serialize html)))
         (content-type "text/html; charset=UTF-8")
         (content-length (length body)))
    (make-instance 'directory-entity
                   :status status
                   :header (header
                            header
                            :content-type content-type
                            :content-length content-length
                            :last-modified last-modified)
                   :body body
                   :html html)))

(defmethod print-object ((entity directory-entity) stream)
  (print-unreadable-object (entity stream :type t :identity t)
    (let ((pathname (entity-body entity)))
      (format stream "~S" pathname))))
