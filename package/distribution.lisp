(in-package :package)

(defclass distribution ()
  ((name
    :initarg :name
    :initform nil
    :reader distribution-name)
   (version
    :initarg :version
    :initform nil
    :reader distribution-version)
   (shasum
    :initarg :shasum
    :initform nil
    :reader distribution-shasum)
   (tarball
    :initarg :tarball
    :initform nil
    :reader distribution-tarball)))

(defmethod print-object ((distribution distribution) stream)
  (print-unreadable-object (distribution stream :type t)
    (format stream "~A" (distribution-tarball distribution))))
