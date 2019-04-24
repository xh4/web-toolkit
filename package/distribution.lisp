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
    :initarg :version
    :initform nil
    :reader distribution-shasum)
   (tarball
    :initarg :tarball
    :initform nil
    :reader distribution-tarball)))
