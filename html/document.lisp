(in-package :html)

(defclass document (dom:document) ())

(defun document (&optional child)
  (check-type child (or html null))
  (make-instance 'document :children (ensure-list child)))
