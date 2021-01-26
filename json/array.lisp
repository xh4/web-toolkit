(in-package :json)

(defclass array ()
  ((value
    :initarg :value
    :initform nil
    :accessor value)))

(defmethod initialize-instance :after ((array array) &key)
  (check-type (value array) list))

(defun array-form (array)
  (check-type array array)
  `(array ,@(value array)))

(defmethod print-object ((array array) stream)
  (prin1 (array-form array) stream))

(defun array (&rest values)
  (make-instance 'array :value values))
