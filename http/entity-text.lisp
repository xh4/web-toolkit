(in-package :http)

(defclass text-entity (entity)
  ((octets
    :initform nil)))

(defmethod initialize-instance :after ((entity text-entity) &key)
  (with-slots (body octets) entity
    (check-type body string)
    (setf octets (babel:string-to-octets body))))

(defmethod content-length ((entity text-entity))
  (with-slots (octets) entity
    (length octets)))

(defmethod content-type ((entity text-entity))
  (or (header-field-value
       (find-header-field "Content-Type" (response-header entity)))
      "text/plain; charset=UTF-8"))

(defmethod response-body ((entity text-entity))
  (with-slots (octets) entity
    octets))
