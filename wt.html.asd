;;;; -*- Mode: LISP -*-

(defsystem wt.html
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :cxml-dom)
  :serial t
  :components ((:module "html"
                        :serial t
                        :components ((:file "package")
                                     (:file "html")))))
