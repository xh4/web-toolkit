;;;; -*- Mode: LISP -*-

(defsystem wt.octicons
  :version "8.4.2"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.component)
  :serial t
  :components ((:module "octicons"
                        :serial t
                        :components ((:file "package")
                                     ))))
