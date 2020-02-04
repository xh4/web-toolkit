(in-package :http)

(defclass file-entity (entity)
  ())

(defmethod initialize-instance :after ((entity file-entity) &key)
  (check-type (entity-body entity) pathname))

(defmethod content-length ((entity text-entity))
  (let ((pathname (entity-body entity)))
    (handler-case
        (with-open-file (stream pathname)
          (file-length stream))
      (error (e)
        (declare (ignore e))
        0))))

(defmethod content-type ((entity text-entity))
  (let ((pathname (entity-body entity)))
    (or (mime-type pathname)
        "application/octet-stream")))

(defmethod last-modified ((entity file-entity))
  (let ((pathname (entity-body entity)))
    (let ((time (or (file-write-date pathname)
                    (get-universal-time))))
      (rfc-1123-date time))))

(defmethod response-status ((entity file-entity))
  (if-let ((status (entity-status entity)))
    status
    (let ((pathname (entity-body entity)))
      (if (probe-file pathname)
          200
          404))))