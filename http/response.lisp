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
