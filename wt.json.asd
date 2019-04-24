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
                                     (:file "decode")
                                     (:file "access")))))

(push
 (merge-pathnames
  "json/cl-json-20141217-git/"
  (asdf:component-pathname
   (asdf:find-system "wt.json")))
 asdf:*central-registry*)
