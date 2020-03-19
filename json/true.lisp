(in-package :json)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass true ()
    ((value
      :initarg :value
      :initform t
      :accessor value))))

(defmethod print-object ((object true) stream)
  (print-unreadable-object (object stream :type t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod make-load-form ((object true) &optional environment)
    `true))

(define-constant true (make-instance 'true)
  :test (lambda (a b)
          (and (typep a 'true)
               (typep b 'true))))

(defun true (&optional (value t))
  (if value true nil))
