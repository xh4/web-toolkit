;;;; -*- Mode: LISP -*-

(defsystem wt.test
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.utility
               :fiveam)
  :defsystem-depends-on (:wt.vendor)
  :serial t
  :components ((:module "test"
                        :serial t
                        :components ((:file "package")
                                     (:file "fiveam")))))
