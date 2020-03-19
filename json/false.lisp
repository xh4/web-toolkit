(in-package :json)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass false ()
    ((value
      :initarg :value
      :initform nil
      :accessor value))))

(defmethod print-object ((object false) stream)
  (print-unreadable-object (object stream :type t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod make-load-form ((object false) &optional environment)
    `false))

(define-constant false (make-instance 'false)
  :test (lambda (a b)
          (and (typep a 'false)
               (typep b 'false))))

(defun false (&optional value)
  (if value value false))
