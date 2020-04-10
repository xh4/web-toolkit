(in-package :css)

(defclass function ()
  ((name
    :initarg :name
    :initform nil
    :accessor function-name)
   (value
    :initarg :value
    :initform nil
    :accessor function-value)))

(define-serialize-method (function stream)
  (format stream "~A" (function-name function))
  (loop for token in (function-value function)
        do (serialize token stream)))