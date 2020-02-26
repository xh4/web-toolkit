(in-package :http)

(defclass form-entity (entity)
  ((form
    :initarg :form
    :initform nil
    :reader entity-form)))

(defun make-form-entity (form &key header status method uri version)
  (check-type form form)
  (let ((content-type (form-content-type form))
        (body (serialize-form form)))
    (make-instance 'form-entity
                   :status (or status 200)
                   :method method
                   :uri uri
                   :version version
                   :header (header
                            :content-type content-type
                            :content-length (length body)
                            header)
                   :body body
                   :form form)))
