(in-package :http)

(defclass html-entity (entity)
  ((string
    :initform nil)))

(defmethod initialize-instance :after ((entity html-entity) &key)
  (with-slots (body string) entity
    (check-type body (or html:document html:element html:text))
    (setf string (html:serialize body))))

(defmethod content-length ((entity html-entity))
  (with-slots (string) entity
    (length string)))

(defmethod content-type ((entity html-entity))
  "text/html; charset=UTF-8")

(defmethod response-status ((entity html-entity))
  200)

(defmethod response-body ((entity html-entity))
  (with-slots (string) entity
    string))
