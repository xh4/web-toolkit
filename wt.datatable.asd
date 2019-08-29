;;;; -*- Mode: LISP -*-

(defsystem wt.datatable
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.component)
  :serial t
  :components ((:module "datatable"
                        :serial t
                        :components ((:file "package")
                                     ))))
