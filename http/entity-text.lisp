(in-package :http)

(defclass text-entity (entity)
  ((text
    :initarg :text
    :initform nil
    :reader entity-text)))

(defun make-text-entity (text &key status header)
  (check-type text string)
  (let ((body (babel:string-to-octets text)))
    (make-instance 'text-entity
                   :status (or status 200)
                   :header (header
                            header
                            :content-type "text/plain; charset=UTF-8"
                            :content-length (length body))
                   :body body
                   :text text)))
