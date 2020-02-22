(in-package :cl-user)

(defpackage :utility
  (:nicknames :wt.utility)
  (:use :cl :alexandria)
  (:export :walk-tree
           :map-tree
           :function-lambda-list))
