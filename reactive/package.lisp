(in-package :cl-user)

(defpackage :reactive
  (:nicknames :rx :wt.reactive :wt.rx)
  (:use :cl :alexandria)
  (:shadow :variable)
  (:export
   ;; object
   :reactive-object
   ;; class
   :reactive-class
   ;; method
   :reactive-method
   ;; variable
   :define-variable
   :variable
   :variable-name
   :variable-form
   :variable-value
   ;; other
   :update
   :reflect
   :add-dependency
   :remove-dependency
   :set-dependency))
