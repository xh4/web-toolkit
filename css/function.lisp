(in-package :css)

(defclass function ()
  ((name
    :initarg :name
    :initform nil
    :accessor function-name)
   (value
    :initarg :value
    :initform nil
    :accessor function-value)))