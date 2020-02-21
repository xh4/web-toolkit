(in-package :dom)

(defclass element (node
                   parent-node
                   non-document-type-child-node
                   child-node)
  ((tag-name
    :initarg :tag-name
    :reader tag-name)
   (id
    :initarg :id
    :reader id)
   (class-name
    :initarg :class-name
    :reader class-name)
   (attributes
    :initarg :attributes
    :reader attributes)))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (tag-name object) stream)))
