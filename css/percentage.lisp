(in-package :css)

(defclass percentage ()
  ((number
    :initarg :number
    :initform nil
    :accessor percentage-number)))

(defmethod print-object ((percentage percentage) stream)
  (print-unreadable-object (percentage stream)
    (format stream "% ~A" (percentage-number percentage))))

(defmethod initialize-instance :after ((percentage percentage) &key)
  (unless (percentage-number percentage)
    (error "Missing number when initialize percentage"))
  (check-type (percentage-number percentage) number))

(defun % (number)
  (make-instance 'percentage :number number))

(defun percentage (value)
  (typecase value
    (string (when-let* ((groups (coerce
                                (nth-value 1 (cl-ppcre:scan-to-strings "([0-9.]+)%" value))
                                'list))
                        (n (parse-integer (first groups) :junk-allowed t)))
              (% n)))
    (percentage value)))
