(in-package :cl-user)

(defpackage :component
  (:nicknames :com :wt.com :wt.component)
  (:use :cl :alexandria)
  (:export
   :define-component
   :render)
  (:import-from :html
                :serialize)
  (:import-from :dom
                :append-child
                :children)
  (:import-from :css
                :style
                :qualified-rule)
  (:import-from :utility
                :rewrite-class-option
                :function-lambda-list)
  (:import-from :reactive
                :reactive-object
                :reactive-class
                :variable-value
                :add-dependency)
  (:shadowing-import-from :reactive
                          :variable)
  (:import-from :uri
                :uri-path)
  (:import-from :closer-mop
                :allocate-instance
                :validate-superclass
                :class-slots
                :class-direct-slots
                :slot-definition
                :slot-definition-name
                :slot-definition-initargs
                :compute-class-precedence-list
                :ensure-finalized
                :slot-value-using-class)
  (:import-from :split-sequence
                :split-sequence))
