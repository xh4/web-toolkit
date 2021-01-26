(in-package :cl-user)

(defpackage :dom
  (:nicknames :wt.dom)
  (:use :cl :alexandria)
  (:shadow :length :append :remove :class-name :type)
  #+sb-package-locks
  (:lock t)
  (:export
   ;; node tree
   :document
   :document-type
   :document-fragment
   :node
   :parent-node
   :child-node
   :element
   :named-node-map
   :attr
   :text
   :processing-instruction
   :comment
   :cdata-section
   ;; node types
   :element-node
   :attribute-node
   :text-node
   :cdata-section-node
   :entity-reference-node
   :entity-node
   :processing-instruction-node
   :comment-node
   :document-node
   :document-type-node
   :document-fragment-node
   ;; namespaces
   :html-namespace
   :mathml-namespace
   :svg-namespace
   :xlink-namespace
   :xml-namespace
   :xmlns-namespace
   ;; old-style collections
   :node-list ;; o
   :html-collection
   :length
   :item
   :named-item
   :node-list-nodes
   ;; node
   :node
   :node-type ;; o
   :node-name ;; o
   :owner-document ;; o
   :parent-node ;; o
   :parent-element
   :has-child-nodes ;; o
   :child-nodes ;; o
   :first-child ;; o
   :last-child ;; o
   :previous-sibling ;; o
   :next-sibling ;; o
   :node-value ;; o TODO: setter
   :text-content ;; - TODO: setter
   :clone-node
   :contains
   :insert-before ;; o
   :append-child ;; o
   :replace-child
   :remove-child
   ;; document
   :document
   :name ;; o
   :public-id ;; o
   :system-id ;; o
   :create-document
   :create-html-document ;; o
   :character-set
   :content-type ;; o
   :doctype ;; o
   :document-element ;; o
   :import-node
   :adopt-node
   ;; document-type
   :document-type
   ;; element
   :element
   :create-element ;; o
   :create-element-ns ;; o
   :namespace ;; o
   :namespace-uri ;; o
   :prefix ;; o
   :local-name ;; o
   :tag-name ;; o
   :id ;; o
   :class-name ;; o
   :class-list
   :closest
   :matches
   :get-elements-by-tag-name
   :get-elements-by-class-name
   :get-element-by-id
   :insert-adjacent-element
   :insert-adjacent-text
   ;; named-node-map
   :named-node-map
   :get-named-item
   :get-named-item-ns
   :set-named-item
   :set-named-item-ns
   :remove-named-item
   :remove-named-item-ns
   ;; attribute
   :attr ;; o
   :name ;; o
   :value ;; o
   :owner-element ;; o
   :has-attributes ;; o
   :attributes ;; o
   :get-attribute-names ;; o
   :get-attribute ;; o
   :get-attribute-ns ;; o
   :set-attribute ;; o
   :set-attribute-ns ;; o
   :remove-attribute ;; o
   :remove-attribute-ns ;; o
   :toggle-attribute ;; o
   :has-attribute ;; o
   :has-attribute-ns ;; o
   :get-attribute-node ;; o
   :get-attribute-node-ns ;; o
   :set-attribute-node ;; o
   :set-attribute-node-ns ;; o
   :remove-attribute-node ;; o
   :create-attribute ;; o
   :create-attribute-ns ;; o
   ;; character-data
   :character-data
   ;; text
   :text
   :data ;; o
   :create-text-node
   :split-text
   :whole-text
   ;; cdata-section
   :cdata-section
   :create-cdata-section
   ;; comment
   :comment
   :create-comment
   ;; traversal
   :create-node-iterator
   :node-iterator
   :next-node
   :previous-node
   :tree-walker
   :create-tree-walker
   :current-node)
  (:import-from :split-sequence
                :split-sequence))
