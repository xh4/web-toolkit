(in-package :http)

(defclass form ()
  ((fields
    :initarg :fields
    :initform nil
    :accessor form-fields)))

(defclass form-field ()
  ((name
    :initarg :name
    :initform nil
    :accessor form-field-name)
   (value
    :initarg :name
    :initform nil
    :accessor form-field-value)))

(defmacro form ()
  )
