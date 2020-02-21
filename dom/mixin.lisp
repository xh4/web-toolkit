(in-package :dom)

(defclass non-element-parent-node () ())

(defgeneric get-element-by-id (node id))

(defclass document-or-shadow-root () ())

(defclass parent-node ()
  ((children
    :initarg :children
    :initform nil
    :accessor children)))

(defgeneric first-element-child (node))

(defgeneric last-element-child (node))

(defgeneric child-element-count (node))

(defgeneric prepend (node nodes))

(defgeneric append (node nodes))

(defgeneric query-selector (node selectors))

(defgeneric query-selector-all (node selectors))

(defclass non-document-type-child-node ()
  ((previous-element-sibling
    :initarg previous-element-sibling
    :initform nil
    :accessor previous-element-sibling)
   (next-element-sibling
    :initarg next-element-sibling
    :initform nil
    :accessor next-element-sibling)))

(defclass child-node () ())

(defgeneric before (node nodes))

(defgeneric after (node nodes))

(defgeneric replace-with (node nodes))

(defgeneric remove (node))
