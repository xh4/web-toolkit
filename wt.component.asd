;;;; -*- Mode: LISP -*-

(defsystem wt.component
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.html
               :alexandria
               :serapeum)
  :serial t
  :components ((:module "component"
                        :serial t
                        :components ((:file "package")
                                     (:file "component")
                                     (:file "render")))))

(defsystem wt.component/test
  :depends-on (:wt.component
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "component"
                                              :components ((:file "package")
                                                           (:file "component")))))))
