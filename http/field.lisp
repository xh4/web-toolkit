(in-package :http)

(defclass field ()
  ((name
    :initarg :name
    :initform nil
    :accessor field-name)
   (value
    :initarg :value
    :initform nil
    :accessor field-value)))

(defmethod print-object ((field field) stream)
  (print-unreadable-object (field stream :type t :identity t)
    (format stream "~S ~S" (field-name field) (field-value field))))

(defgeneric fields (object))

(defgeneric (setf fields) (fields object))

(defmacro field (name value)
  `(make-instance 'field :name ,name :value ,value))
