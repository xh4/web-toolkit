(in-package :http)

(defclass file-entity (entity)
  ((file
    :initarg :file
    :initform nil
    :accessor entity-file)))

(defmethod initialize-instance :after ((entity file-entity) &key)
  (check-type (entity-body entity) pathname)
  (let ((pathname (entity-body entity)))
    (when (directory-pathname-p pathname)
      (error "Pathname ~S should denote a file" pathname))
    (let ((file (make-instance 'file :pathname pathname)))
      (setf (entity-file entity) file))))

(defmethod print-object ((entity file-entity) stream)
  (print-unreadable-object (entity stream :type t :identity t)
    (let ((pathname (entity-body entity)))
      (format stream "~S" pathname))))

(defmethod content-length ((entity file-entity))
  (let ((pathname (entity-body entity)))
    (handler-case
        (with-open-file (stream pathname)
          (file-length stream))
      (error (e)
        (declare (ignore e))
        0))))

(defmethod content-type ((entity file-entity))
  (let ((pathname (entity-body entity)))
    (or (mime-type pathname)
        "application/octet-stream")))

(defmethod last-modified ((entity file-entity))
  (let ((file (entity-file entity)))
    (let ((modify-time (file-modify-time file)))
      (rfc-1123-date modify-time))))

(defmethod entity-status ((entity file-entity))
  (let ((pathname (entity-body entity)))
    (if (probe-file pathname)
        200
        404)))
