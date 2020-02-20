(in-package :component)

(declaim (inline reuse-cons))
(defun reuse-cons (x y x-y)
  (if (and (eq x (car x-y))
           (eq y (cdr x-y)))
      x-y
      (cons x y)))

(defun map-tree (fun tree &key (tag nil tagp))
  (labels ((map-tree (tree)
             (let ((tree2 (funcall fun tree)))
               (if (atom tree2)
                   tree2
                   (reuse-cons (map-tree (car tree2))
                               (map-tree (cdr tree2))
                               tree2))))
           (map-tree/tag (tree tag)
             (catch tag
               (let ((tree2 (funcall fun tree)))
                 (if (atom tree2)
                     tree2
                     (reuse-cons (map-tree/tag (car tree2) tag)
                                 (map-tree/tag (cdr tree2) tag)
                                 tree2))))))
    (if tagp
        (map-tree/tag tree tag)
        (map-tree tree))))
