;;;; -*- Mode: LISP -*-

(defsystem wt.html
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :cxml-dom
               :closure-html)
  :serial t
  :components ((:module "html"
                        :serial t
                        :components ((:file "package")
                                     (:file "html")
                                     (:file "serialize")))))
