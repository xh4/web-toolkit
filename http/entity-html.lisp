(in-package :http)

(defclass html-entity (entity)
  ((string
    :initarg :string
    :initform nil)
   (html
    :initarg :html
    :initform nil
    :reader entity-html)))

(defun make-html-entity (html &key header status method uri version)
  (check-type html (or html:document html:element))
  (let ((string (html:serialize html)))
    (let ((body (babel:string-to-octets string)))
      (make-instance 'html-entity
                     :status (or status 200)
                     :method method
                     :uri uri
                     :version version
                     :header (header
                              :content-length (length body)
                              :content-type "text/html; charset=UTF-8"
                              header)
                     :body body
                     :string string
                     :html html))))
