(in-package :package)

(defclass registry ()
  ((endpoint
    :initarg :endpoint
    :initform "https://registry.npmjs.org"
    :accessor registry-endpoint)
   (metadata
    :initarg :metadata
    :initform (make-hash-table :test 'equal)
    :accessor registry-metadata)))

(defvar *registry* (make-instance 'registry))
