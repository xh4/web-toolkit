(in-package :dom)

(defclass non-element-parent-node () ())

(defgeneric get-element-by-id (node id))

(defclass document-or-shadow-root () ())

(defclass parent-node ()
  ((children
    :initarg :children
    :initform nil
    :accessor children)))

(defgeneric first-child (node))

(defgeneric last-child (node))

(defgeneric child-count (node))

(defgeneric prepend (node nodes))

(defgeneric append (node nodes))

(defgeneric query-selector (node selectors))

(defgeneric query-selector-all (node selectors))

(defclass non-document-type-child-node ()
  ((previous-sibling
    :initarg previous-sibling
    :initform nil
    :accessor previous-sibling)
   (next-sibling
    :initarg next-sibling
    :initform nil
    :accessor next-sibling)))

(defclass child-node ()
  ((parent
    :initarg :parent
    :initform nil
    :accessor parent)))

(defgeneric before (node nodes))

(defgeneric after (node nodes))

(defgeneric replace-with (node nodes))

(defgeneric remove (node))
