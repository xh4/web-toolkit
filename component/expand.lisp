(in-package :component)

(defgeneric expand (component))


(defun expand-1 (component)
  (expand component))


(defun expand-all (component)
  (let ((root (expand component)))
    (serapeum:map-tree
     (lambda (form)
       (if (and (atom form) (typep form 'component))
           (expand-all form)
           form))
     root)))
