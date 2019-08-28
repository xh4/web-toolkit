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

(defsystem wt.html/test
  :depends-on (:wt.html
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "html"
                                              :components ((:file "package")
                                                           (:file "html")))))))
