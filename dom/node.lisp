(in-package :dom)

(defclass node ()
  ((parent
    :initarg :parent
    :initform nil
    :accessor parent)
   (children
    :initarg :children
    :initform nil
    :accessor children)))

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

(defgeneric insert-before (node child))

(defgeneric append-child (node child)
  (:method ((node node) child)
    (with-slots (children) node
      (appendf children (list child))
      (setf (parent child) node)))
  (:method ((node node) (child null))

(defgeneric replace-child (node child))

(defgeneric remove-child (node child))
