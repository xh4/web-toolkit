;;;; -*- Mode: LISP -*-

(defsystem wt.http
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on ()
  :serial t
  :components ((:module "http"
                        :serial t
                        :components ((:file "package")
                                     ))))
