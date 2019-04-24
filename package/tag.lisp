(in-package :package)

(defclass tag ()
  ((name
    :initarg :name
    :initform nil
    :reader tag-name)
   (version
    :initarg :version
    :initform nil
    :reader tag-version)))

(defclass beta (tag) ((name :initform "beta")))

(defclass latest (tag) ((name :initform "latest")))
