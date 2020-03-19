(in-package :json)

(defclass array ()
  ((value
    :initarg :value
    :initform nil
    :accessor value)))

(defmethod print-object ((array array) stream)
  (print-unreadable-object (array stream :type t)
    (with-slots (value) array
      (format stream "~A" value))))

(defun array (&optional value)
  (typecase value
    (string (error "Can't build JSON array from value ~S" value))
    (vector (setf value (coerce value 'list)))
    (list)
    (t (error "Can't build JSON array from value ~A" value)))
  (make-instance 'array :value value))
