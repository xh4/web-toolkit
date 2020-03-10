(in-package :json)

(defclass maybe-null ()
  ((value
    :initarg :value
    :initform nil
    :accessor value)))

(defmethod print-object ((maybe-null maybe-null) stream)
  (print-unreadable-object (maybe-null stream :type t)
    (with-slots (value) maybe-null
      (format stream "(~A)" value))))

(defun maybe-null (value)
  (make-instance 'maybe-null :value value))

(defclass null (maybe-null) ())

(defmethod print-object ((null null) stream)
  (print-unreadable-object (null stream :type t)))

(define-constant null (make-instance 'null)
  :test (lambda (a b)
          (and (typep a 'null)
               (typep b 'null))))

(defun null (&optional value)
  (when value
    (error "Can't build JSON null from value ~A" value))
  (make-instance 'null))
