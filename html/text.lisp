(in-package :html)

(defclass text (dom:text) ())

(defmethod print-object ((text text) stream)
  (print-unreadable-object (text stream :type t :identity t)))

(defun text (&optional data)
  (check-type data (or null string))
  (make-instance 'text :data data))
