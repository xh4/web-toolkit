;;;; -*- Mode: LISP -*-

(defsystem wt.bootstrap
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.component
               :trivia)
  :serial t
  :components ((:module "bootstrap"
                        :serial t
                        :components ((:file "package")
                                     (:file "button")))))
