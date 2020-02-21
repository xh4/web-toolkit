(in-package :cl-user)

(defpackage :html
  (:nicknames :wt.html)
  (:use :cl :alexandria)
  (:shadow :time :map)
  (:export :document
           :document-title
           :document-body
           :document-head
           :document-images
           :document-links
           :document-forms
           :document-scripts
           :element
           :element-title
           :element-lang
           :element-translate
           :element-dir
           :text
           :serialize))
