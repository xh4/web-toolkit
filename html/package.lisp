(in-package :cl-user)

(defpackage :html
  (:nicknames :wt.html)
  (:use :cl :alexandria)
  (:shadow :time :map)
  (:export :document
           :text
           :serialize))
