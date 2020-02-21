(in-package :dom)

(defclass document (node
                    non-element-parent-node
                    document-or-shadow-root
                    parent-node)
  ())
