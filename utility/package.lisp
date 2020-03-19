(in-package :cl-user)

(defpackage :utility
  (:nicknames :wt.utility)
  (:use :cl :alexandria)
  (:export
   ;; tree
   :walk-tree
   :map-tree
   ;; string
   :string-prefix-p
   :string-suffix-p
   ;; function
   :function-lambda-list
   ;; class
   :replace-class-option
   :rewrite-class-option
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
