;;;; -*- Mode: LISP -*-

(defsystem wt.website
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.http
               :wt.bootstrap)
  :serial t
  :components ((:module "website"
                        :serial t
                        :components ((:file "package")
                                     (:file "server")))))
