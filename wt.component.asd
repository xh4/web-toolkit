;;;; -*- Mode: LISP -*-

(defsystem wt.component
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria)
  :serial t
  :components ((:module "component"
                        :serial t
                        :components ((:file "package")
                                     ))))
