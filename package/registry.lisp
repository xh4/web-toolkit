(in-package :package)

(defclass registry ()
  ((endpoint
    :initarg :endpoint
    :initform "https://registry.npmjs.org"
    :accessor registry-endpoint)))

(defparameter *registry*
  (make-instance 'registry))
