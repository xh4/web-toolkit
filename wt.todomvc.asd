;;;; -*- Mode: LISP -*-

(defsystem wt.todomvc
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :defsystem-depends-on (:wt.package)
  :depends-on ()
  :serial t
  :components ((:package "todomvc")
               (:module "todomvc"
                        :serial t
                        :components ((:file "package")
                                     ))))
