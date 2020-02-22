(in-package :cl-user)

(defpackage :dom
  (:nicknames :wt.dom)
  (:use :cl :alexandria)
  (:shadow :length :append :remove)
  (:export :document
           :node
           :root
           :append-child
           :first-child
           :last-child
           :sibling
           :index
           :previous-sibling
           :next-sibling
           :preceding
           :following
           :element
           :element-class
           :has-attributes
           :get-attribute-names
           :get-attribute
           :set-attribute
           :remove-attribute
           :toggle-attribute
           :has-attribute
           :text
           :create-node-iterator
           :node-iterator
           :next-node
           :previous-node
           :tree-walker
           :current-node))
