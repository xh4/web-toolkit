(in-package :dom)

(defclass node () ())

(defgeneric node-type (node))

(defgeneric node-name (node))

(defgeneric parent-node (node))

(defgeneric parent-element (node))

(defgeneric has-child-nodes-p (node))

(defgeneric child-nodes (node))

(defmethod root ((node node))
  (if-let ((parent (parent node)))
    (root parent)
    node))

(defmethod first-child ((node node))
  (first (children node)))

(defmethod last-child ((node node))
  (car (last (children node))))

(defmethod sibling ((node node))
  (when-let ((parent (parent node)))
    (cl:remove node (children parent))))

(defmethod index ((node node))
  (if-let ((parent (parent node)))
    (or (position node (children parent)) 0)
    0))

(defmethod previous-sibling ((node node))
  (when-let ((parent (parent node)))
    (let ((index (index node)))
      (if (> index 0)
          (nth (1- index) (children parent))))))

(defmethod next-sibling ((node node))
  (when-let ((parent (parent node)))
    (let ((index (index node)))
      (if (< index (1- (cl:length (children parent))))
          (nth (1+ index) (children parent))))))

(defmethod preceding ((node node))
  (let ((index (index node)))
    (labels ((down (node)
               (if-let ((last-child (last-child node)))
                 (down last-child)
                 node)))
      (if (= index 0)
          (parent node)
          (down (previous-sibling node))))))

(defmethod following ((node node))
  (labels ((up (node)
              (if-let ((parent (parent node)))
                (or (next-sibling parent)
                    (up parent)))))
    (if (children node)
        (first (children node))
        (or (next-sibling node)
            (up node)))))

(defgeneric node-value (node))

(defgeneric text-content (node))

(defgeneric normalize (node))

(defgeneric node-equal-p (node))

(defgeneric clone-node (node))

(defgeneric contains-p (node other))

(defgeneric insert-before (parent node child)
  (:method ((parent node) (node node) (child node))
    (let ((i (position child (children parent))))
      (unless i
        (error "Child ~A not found" child))
      (setf (parent node) parent)
      (setf (children parent)
            (cl:append (subseq (children parent) 0 i)
                       (cons node
                             (subseq (children parent) i)))))
    node)
  (:method ((parent node) (node node) (child null))
    (setf (parent node) parent)
    (setf (children parent) (cons node (children parent)))
    node))

(defgeneric append-child (node child)
  (:method ((node node) (child node))
    (with-slots (children) node
      (appendf children (list child))
      (setf (parent child) node))
    child))

(defgeneric replace-child (node child))

(defgeneric remove-child (node child))
