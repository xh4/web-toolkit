(in-package :http)

(defclass file-entity (entity)
  ((file
    :initarg :file
    :initform nil
    :reader entity-file)))

(defmethod print-object ((entity file-entity) stream)
  (print-unreadable-object (entity stream :type t :identity t)
    (let ((pathname (entity-body entity)))
      (format stream "~S" pathname))))

(defun make-file-entity (pathname &key status header)
  (check-type pathname pathname)
  (when (directory-pathname-p pathname)
    (error "Pathname ~S should denote a file" pathname))
  (let* ((file (make-instance 'file :pathname pathname))
         (content-type (or (mime-type pathname)
                           "application/octet-stream"))
         (content-length (or (file-size file) 0))
         (last-modified (rfc-1123-date (file-modify-time file)))
         (status (or status
                     (if (probe-file pathname)
                         200
                         404)))
         (body pathname))
    (make-instance 'file-entity
                   :status status
                   :header (header
                            header
                            :content-type content-type
                            :content-length content-length
                            :last-modified last-modified)
                   :body body
                   :file file)))
