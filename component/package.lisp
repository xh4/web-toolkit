(in-package :cl-user)

(defpackage :component
  (:nicknames :com :wt.com :wt.component)
  (:use :cl :alexandria :utility)
  (:shadow :variable)
  (:export :id
           :define-component
           :render
           :serialize
           :root
           :children
           :append-child
           :define-variable
           :variable)
  (:import-from :html
                :append-child
                :serialize
                :children)
  (:import-from :closer-mop
                :allocate-instance
                :validate-superclass
                :class-slots
                :slot-definition-name
                :slot-definition-initargs
                :compute-class-precedence-list
                :ensure-finalized)
  (:import-from :split-sequence
                :split-sequence))
