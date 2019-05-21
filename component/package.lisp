(in-package :cl-user)

(defpackage :component
  (:nicknames :com :wt.com :wt.component)
  (:use :cl :alexandria)
  (:export :define-component
           :component-children
           :expand
           :expand-1
           :expand-all
           :button
           :text))
