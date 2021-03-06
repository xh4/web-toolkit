(in-package :cl-user)

(defpackage :component
  (:nicknames #-capi :com :wt.com :wt.component)
  (:use :cl :alexandria)
  (:export
   :define-component
   :component
   :render
   :render-all
   :diff
   :component-class-style)
  (:import-from :html
                :serialize)
  (:import-from :dom
                :append-child
                :children)
  (:import-from :utility
                :rewrite-class-option
                :function-lambda-list)
  (:import-from :reactive
                :reactive-object
                :reactive-class
                :variable-value
                :with-variable-capturing
                :object-propagation
                :add-dependency
                :react
                :with-propagation
                :without-propagation)
  (:shadowing-import-from :reactive
                          :variable)
  (:import-from :uri
                :uri-path)
  (:import-from :closer-mop
                :allocate-instance
                :validate-superclass
                :class-slots
                :class-direct-slots
                :class-direct-subclasses
                :slot-definition
                :slot-definition-name
                :slot-definition-initargs
                :compute-class-precedence-list
                :ensure-finalized
                :slot-value-using-class)
  (:import-from :split-sequence
                :split-sequence))
