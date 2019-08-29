(in-package :form)

(define-component avatar-field (field)
  ())

(define-component avatar-control (control)
  ((file
    :initarg :file
    :initform nil
    :accessor control-file)
   (scale
    :initarg :scale
    :initform nil
    :accessor control-scale)
   (left
    :initarg :left
    :initform nil
    :accessor control-left)
   (top
    :initarg :top
    :initform nil
    :accessor control-top)))
