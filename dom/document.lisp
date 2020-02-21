(in-package :dom)

(defclass document (node
                    non-element-parent-node
                    document-or-shadow-root
                    parent-node)
  ())

(defclass document-type (node
                         child-node)
  ())

(defclass document-fragment (node
                             non-element-parent-node
                             parent-node)
  ())
