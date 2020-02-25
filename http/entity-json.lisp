(in-package :http)

(defclass json-entity (entity)
  ((string
    :initarg :string
    :initform nil)
   (json
    :initarg :json
    :initform nil
    :reader entity-json)))

(defun make-json-entity (json &key header status method uri version)
  (check-type json (or string number null list json:object))
  (let ((string (json:encode json)))
    (let ((body (babel:string-to-octets string)))
      (make-instance 'json-entity
                     :status (or status 200)
                     :method method
                     :uri uri
                     :version version
                     :header (header
                              :content-length (length body)
                              :content-type "application/json; charset=UTF-8"
                              header)
                     :body body
                     :string string
                     :json json))))
