(in-package :cl-user)

(defpackage :utility
  (:nicknames :wt.utility)
  (:use :cl :alexandria)
  (:shadow :variable)
  (:export
   ;; tree
   :walk-tree
   :map-tree
   ;; function
   :function-lambda-list
   ;; class
   :replace-class-option
   :rewrite-class-option
   ;; variable
   :define-variable
   :variable
   :variable-name
   :variable-form
   :variable-value
   ;; reactive
   :reactive-object
   :reactive-class
   :reactive-method
   :reactive-object-p
   :update
   :reflect
   :add-dependency
   :remove-dependency
   :set-dependency
   ;; parser
   :parser
   :define-parser
   :parse
   :*parser-stack*
   :with-parser-stack
   :parser-match-all-p
   :parser-value
   :.element :.satisfies :.or :.test :.eq :.seq :.seq/s
   :.any :.any/s :.and :.maybe :.some :.some/s :.end :.not
   :.n :.n/s :.m :.m/s :.s
   :.alpha :alpha-p :.digit :digit-p :.hexdig))
