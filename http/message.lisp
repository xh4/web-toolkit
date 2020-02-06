(in-package :http)

(defclass message ()
  ((header
    :initarg :header
    :initform nil
    :accessor message-header)
   (body
    :initarg :body
    :initform nil
    :accessor message-body)))

(defgeneric message-body-present-p (message)
  (:method ((message message))
    (or (find-header-field "Content-Length" message)
        (find-header-field "Transfer-Encoding" message))))

(defgeneric transfer-encoding-chunked-p (message)
  (:method ((message message))
    (search "chunked" (header-field-value
                       (find-header-field "Transfer-Encoding" message)))))
