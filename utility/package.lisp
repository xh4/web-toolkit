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
   ;; reflective
   :reflective-object
   :reflective-class
   :reflective-method
   :reflective-object-p
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
   :.n :.n/s :.s))
