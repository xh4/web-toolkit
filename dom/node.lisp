(in-package :dom2)

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

(defgeneric first-child (node))

(defgeneric last-child (node))

(defgeneric previous-sibling (node))

(defgeneric next-sibling (node))

(defgeneric node-value (node))

(defgeneric text-content (node))

(defgeneric normalize (node))

(defgeneric node-equal-p (node))

(defgeneric clone-node (node))

(defgeneric contains-p (node other))

(defgeneric insert-before (node child))

(defgeneric append-child (node))

(defgeneric replace-child (node child))

(defgeneric remove-child (node child))
