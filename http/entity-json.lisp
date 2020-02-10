(in-package :http)

(defclass json-entity (entity)
  ((string
    :initform nil)))

(defmethod initialize-instance :after ((entity json-entity) &key)
  (with-slots (body string) entity
    (check-type body (or string number null list json:object))
    (setf string (json:encode body))))

(defmethod content-length ((entity json-entity))
  (with-slots (string) entity
    (length string)))

(defmethod content-type ((entity json-entity))
  "application/json; charset=UTF-8")

(defmethod response-body ((entity json-entity))
  (with-slots (string) entity
    string))
