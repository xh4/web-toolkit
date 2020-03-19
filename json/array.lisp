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

(defun array (&rest values)
  (if (= 1 (length values))
      (let ((value (first values)))
        (cond
          ((and (vectorp value)
                (not (stringp value)))
           (make-instance 'array :value (coerce value 'list)))
          ((listp value)
           (make-instance 'array :value value))
          (t (error "Can't build JSON array from value ~A" value))))
      (funcall #'array values)))
