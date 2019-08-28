;;;; -*- Mode: LISP -*-

(defsystem wt.uri
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:quri)
  :serial t
  :components ((:module "uri"
                        :serial t
                        :components ((:file "package")
                                     (:file "uri")))))

(defsystem wt.uri/test
  :depends-on (:wt.uri
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "uri"
                                              :components ((:file "package")
                                                           (:file "uri")))))))
