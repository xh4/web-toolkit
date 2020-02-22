(in-package :utility)

(declaim (inline reuse-cons))
(defun reuse-cons (x y x-y)
  "If X and Y are the car and cdr of X-Y, return X-Y.
Otherwise, return a fresh cons of X and Y."
  (if (and (eq x (car x-y))
           (eq y (cdr x-y)))
      x-y
      (cons x y)))

(defun walk-tree (fun tree &key (tag nil tagp) (traversal :preorder))
  "Call FUN in turn over each atom and cons of TREE.
FUN can skip the current subtree with (throw TAG nil)."
  (let ((fun (ensure-function fun)))
    ;; NB map-tree only conses if you change something.
    (multiple-value-call #'map-tree
      (lambda (tree)
        (funcall fun tree)
        tree)
      tree
      :traversal traversal
      (if tagp (values :tag tag) (values)))
    (values)))

(defun map-tree (fun tree &key (tag nil tagp)
                            (traversal :preorder))
  "Walk FUN over TREE and build a tree from the results.
The new tree may share structure with the old tree.
     (eq tree (map-tree #'identity tree)) => T
FUN can skip the current subtree with (throw TAG SUBTREE), in which
case SUBTREE will be used as the value of the subtree.
TRAVERSE can be one of `:preorder', `:postorder', or `:inorder'. The
default is `:preorder'."
  #.+merge-tail-calls+
  (ecase traversal
    (:preorder (map-tree/preorder fun tree tag tagp))
    (:postorder (map-tree/postorder fun tree tag tagp))
    (:inorder (map-tree/inorder fun tree tag tagp))))

(defun map-tree/preorder (fun tree tag tagp)
  #.+merge-tail-calls+
  (let ((fun (ensure-function fun)))
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
          (map-tree tree)))))

(defun map-tree/postorder (fun tree tag tagp)
  #.+merge-tail-calls+
  (let ((fun (ensure-function fun)))
    (labels ((map-tree (tree)
               (if (atom tree)
                   (funcall fun tree)
                   (let* ((left (map-tree (car tree)))
                          (right (map-tree (cdr tree)))
                          (tree2 (reuse-cons left right tree)))
                     (funcall fun tree2))))
             (map-tree/tag (tree tag)
               (catch tag
                 (if (atom tree)
                     (funcall fun tree)
                     (let* ((left (map-tree/tag (car tree) tag))
                            (right (map-tree/tag (cdr tree) tag))
                            (tree2 (reuse-cons left right tree)))
                       (funcall fun tree2))))))
      (if tagp
          (map-tree/tag tree tag)
          (map-tree tree)))))

(defun map-tree/inorder (fun tree tag tagp)
  #.+merge-tail-calls+
  (let ((fun (ensure-function fun)))
    (labels ((map-tree (tree)
               (if (atom tree)
                   (funcall fun tree)
                   (let* ((left (map-tree (car tree)))
                          (tree2 (funcall fun (reuse-cons left (cdr tree) tree))))
                     (reuse-cons (car tree2)
                                 (map-tree (cdr tree2))
                                 tree2))))
             (map-tree/tag (tree tag)
               (catch tag
                 (if (atom tree)
                     (funcall fun tree)
                     (let* ((left (map-tree/tag (car tree) tag))
                            (tree2 (funcall fun (reuse-cons left (cdr tree) tree))))
                       (reuse-cons (car tree2)
                                   (map-tree/tag (cdr tree2) tag)
                                   tree2))))))
      (if tagp
          (map-tree/tag tree tag)
          (map-tree tree)))))
