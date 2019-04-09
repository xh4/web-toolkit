;;;; -*- Mode: LISP -*-

(defsystem wt.json
  :version "0.9.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:cl-json)
  :serial t
  :components ((:module "json"
                        :serial t
                        :components ((:file "package")
                                     (:file "object")
                                     (:file "encode")
                                     (:file "decode")))))
