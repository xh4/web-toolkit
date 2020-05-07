(in-package :cl-user)

(defpackage :dom
  (:nicknames :wt.dom)
  (:use :cl :alexandria)
  (:shadow :length :append :remove :append)
  #+sb-package-locks
  (:lock t)
  (:export
   ;; document
   :document
   ;; node
   :node
   :parent-node
   :child-node
   :root
   :parent
   :children
   :append-child
   :first-child
   :last-child
   :sibling
   :index
   :previous-sibling
   :next-sibling
   :preceding
   :following
   :insert-before
   :clone-node
   ;; element
   :element
   :tag-name
   :has-attributes
   :get-attribute-names
   :get-attribute
   :set-attribute
   :remove-attribute
   :toggle-attribute
   :has-attribute
   ;; text
   :text
   :data
   ;; traversal
   :create-node-iterator
   :node-iterator
   :next-node
   :previous-node
   :tree-walker
   :current-node))
