(in-package :html)

(defclass text (dom:text) ())

(defun text (&optional data)
  (check-type data (or null string))
  (make-instance 'text :data data))
