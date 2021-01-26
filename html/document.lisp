(in-package :html)

(defclass document (dom:document) ())

(defun document (&optional child)
  (check-type child (or html null))
  (let ((document (make-instance 'document)))
    (prog1 document
      (when child
        (dom:append-child document child)))))
