(in-package :http)

(defclass text-entity (entity)
  ())

(defmethod initialize-instance :after ((entity text-entity) &key)
  (check-type (entity-body entity) string))

(defmethod content-length ((entity text-entity))
  (let ((string (entity-body entity)))
    (length string)))

(defmethod content-type ((entity text-entity))
  (or (header-field-value
       (find-header-field "Content-Type" (response-header entity)))
      "text/plain; charset=UTF-8"))

(defmethod response-status ((entity text-entity))
  200)
