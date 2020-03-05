(in-package :cl-user)

(defpackage :utility
  (:nicknames :wt.utility)
  (:use :cl :alexandria)
  (:shadow :variable)
  (:export :walk-tree
           :map-tree
           :function-lambda-list
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
           :set-dependency))
