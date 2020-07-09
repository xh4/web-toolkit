(in-package :css)

(defclass stylesheet ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor stylesheet-rules)))

(defun stylesheet (&rest rules)
  (loop for rule in rules
     when (typep rule 'rule)
     collect rule into rules-1
     finally (return (make-instance 'stylesheet
                                    :rules rules-1))))

(define-serialize-method ((stylesheet stylesheet) stream)
  (loop for rule in (stylesheet-rules stylesheet)
     do (progn
          (serialize rule stream)
          (format stream "~%"))))
