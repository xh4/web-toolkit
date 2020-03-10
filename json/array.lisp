(in-package :json)

(defclass maybe-array ()
  ((value
    :initarg :value
    :initform nil
    :accessor value)))

(defmethod print-object ((maybe-array maybe-array) stream)
  (print-unreadable-object (maybe-array stream :type t)
    (with-slots (value) maybe-array
      (format stream "~A" value))))

(defun maybe-array (value)
  (make-instance 'maybe-array :value value))

(defclass array (maybe-array) ())

(defmethod print-object ((array array) stream)
  (print-unreadable-object (array stream :type t)
    (with-slots (value) array
      (format stream "~A" value))))

(defun array (&optional value)
  (unless (listp value)
    (error "Can't build JSON array from value ~A" value))
  (make-instance 'array :value value))
