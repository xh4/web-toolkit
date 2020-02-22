;;;; -*- Mode: LISP -*-

(defsystem wt.utility
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :serial t
  :depends-on (:alexandria
               (:feature :sbcl (:require :sb-introspect)))
  :components ((:module "utility"
                        :serial t
                        :components ((:file "package")
                                     (:file "macro")
                                     (:file "function")
                                     (:file "tree")))))
