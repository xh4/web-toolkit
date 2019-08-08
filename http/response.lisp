(in-package :http)

(defclass response ()
  ((status
    :initarg :status
    :initform nil
    :accessor response-status)
   (header
    :initarg :header
    :initform nil
    :accessor response-header)
   (body
    :initarg :body
    :initform nil
    :accessor response-body)))

(defvar *response* nil)

(defgeneric response-status (response))

(defgeneric (setf response-status) (value response))

(defgeneric response-header (response))

(defgeneric (setf response-header) (value response))

(defgeneric response-body (response))

(defgeneric (setf response-body) (value response))

(defmethod header-fields ((response response))
  (let ((header (response-header response)))
    (header-fields header)))

(defmethod (setf header-fields) (value (response response))
  (let ((header (response-header response)))
    (setf (header-fields header) value)))

(defmethod header-field ((response response) name)
  (let ((header (response-header response)))
    (header-field header name)))

(defmethod (setf header-field) (value (response response) name)
  (let ((header (response-header response)))
    (setf (header-field header name) value)))
