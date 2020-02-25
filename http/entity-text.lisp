(in-package :http)

(defclass text-entity (entity)
  ((text
    :initarg :text
    :initform nil
    :reader entity-text)))

(defun make-text-entity (text &key header status method uri version)
  (check-type text string)
  (let ((body (babel:string-to-octets text)))
    (make-instance 'text-entity
                   :status (or status 200)
                   :method method
                   :uri uri
                   :version version
                   :header (header
                            :content-type "text/plain; charset=UTF-8"
                            :content-length (length body)
                            header)
                   :body body
                   :text text)))
