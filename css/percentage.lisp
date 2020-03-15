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

(define-parser .percentage ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.seq/s (.m/s 3 (.digit)) (.s "%")) input)
      (if match-p
          (let ((n (parse-integer (subseq value 0 (1- (cl:length value))))))
            (if (<= 0 n 100)
                (values rest (% n) t)
                (values input nil nil)))
          (values input nil nil)))))

(defun percentage (value)
  (typecase value
    (string (when-let* ((groups (coerce
                                (nth-value 1 (cl-ppcre:scan-to-strings "^([0-9.]+)%$" value))
                                'list))
                        (n (parse-integer (first groups) :junk-allowed t)))
              (% n)))
    (percentage value)))
