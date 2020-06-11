(in-package :dom)

(defclass attribute ()
  ((name
    :initarg :name
    :initform nil
    :accessor attribute-name)
   (value
    :initarg :value
    :initform nil
    :accessor attribute-value)))