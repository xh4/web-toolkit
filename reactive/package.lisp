(in-package :cl-user)

(defpackage :reactive
  (:nicknames :rx :wt.reactive :wt.rx)
  (:use :cl :alexandria)
  (:shadow :variable)
  (:export
   ;; object
   :reactive-object
   :object-dependency
   :object-propagation
   ;; class
   :reactive-class
   ;; propagate
   :with-propagation
   :without-propagation
   ;; react
   :react
   ;; method
   :reactive-method
   ;; variable
   :define-variable
   :variable
   :variable-name
   :variable-form
   :variable-value
   ;; other
   :add-dependency)
  (:import-from :utility
                :rewrite-class-option)
  (:import-from :closer-mop
                :validate-superclass
                :slot-value-using-class
                :class-direct-superclasses
                :class-direct-subclasses
                :add-direct-subclass
                :slot-definition
                :slot-definition-name)
  (:import-from :trivial-garbage
                :finalize
                :make-weak-pointer
                :weak-pointer-value))
