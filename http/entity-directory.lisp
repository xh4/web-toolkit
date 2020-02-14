(in-package :http)

(defclass directory-entity (entity)
  ((directory
    :initarg :directory
    :initform nil
    :accessor entity-directory)))

(defmethod initialize-instance :after ((entity directory-entity) &key)
  (check-type (entity-body entity) pathname)
  (let ((pathname (entity-body entity)))
    (unless (directory-pathname-p pathname)
      (error "Pathname ~S should denote a directory" pathname))
    (let ((directory (make-instance 'directory :pathname pathname)))
      (setf (entity-directory entity) directory))))

(defmethod print-object ((entity directory-entity) stream)
  (print-unreadable-object (entity stream :type t :identity t)
    (let ((pathname (entity-body entity)))
      (format stream "~S" pathname))))

(defmethod last-modified ((entity directory-entity))
  (let ((directory (entity-directory entity)))
    (let ((modify-time (directory-modify-time directory)))
      (rfc-1123-date modify-time))))

(defmethod entity-status ((entity directory-entity))
  (let ((pathname (entity-body entity)))
    (if (probe-file pathname)
        200
        404)))
