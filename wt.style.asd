;;;; -*- Mode: LISP -*-

(defsystem wt.style
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               )
  :serial t
  :components ((:module "style"
                        :serial t
                        :components ((:file "package")
                                     (:file "color")
                                     (:file "syntax")))))
