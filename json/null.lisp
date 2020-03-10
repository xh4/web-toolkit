(in-package :json)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass null ()
    ((value
      :initarg :value
      :initform nil
      :accessor value))))

(defmethod print-object ((object null) stream)
  (print-unreadable-object (object stream :type t)))

(defmethod make-load-form ((object null) &optional environment)
  `null)

(define-constant null (make-instance 'null)
  :test (lambda (a b)
          (and (typep a 'null)
               (typep b 'null))))

(defun null (&optional value)
  (if value value null))
