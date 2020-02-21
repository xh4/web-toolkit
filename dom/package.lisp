(in-package :cl-user)

(defpackage :dom
  (:nicknames :wt.dom)
  (:use :cl :alexandria)
  (:shadow :length :append :remove)
  (:export :document
           :node
           :append-child
           :element
           :element-class
           :has-attributes
           :get-attribute-names
           :get-attribute
           :set-attribute
           :remove-attribute
           :toggle-attribute
           :has-attribute
           :text))
