(in-package :http)

(defclass html-entity (entity)
  ((string
    :initform nil)
   (octets
    :initform nil)))

(defmethod initialize-instance :after ((entity html-entity) &key)
  (with-slots (body string octets) entity
    (check-type body (or html:document html:element html:text))
    (setf string (html:serialize body)
          octets (babel:string-to-octets string))))

(defmethod content-length ((entity html-entity))
  (with-slots (octets) entity
    (length octets)))

(defmethod content-type ((entity html-entity))
  "text/html; charset=UTF-8")

(defmethod response-body ((entity html-entity))
  (with-slots (octets) entity
    octets))
