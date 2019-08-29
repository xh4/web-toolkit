(in-package :form)

(define-component control ()
  ((name
    :initarg :name
    :initform nil
    :accessor control-name)
   (value
    :initarg :value
    :initform nil
    :accessor control-value)))
