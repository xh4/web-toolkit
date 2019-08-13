;;;; -*- Mode: LISP -*-

(defsystem wt.json
  :version "0.9.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:cl-json
               :alexandria)
  :serial t
  :components ((:module "json"
                        :serial t
                        :components ((:file "package")
                                     (:file "object")
                                     (:file "access")
                                     (:file "encode")
                                     (:file "decode")))))

(defsystem wt.json/test
  :depends-on (:wt.json
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "json"
                                              :components ((:file "package")
                                                           ))))))

(push
 (merge-pathnames
  "json/cl-json-20141217-git/"
  (asdf:component-pathname
   (asdf:find-system "wt.json")))
 asdf:*central-registry*)
