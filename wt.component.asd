;;;; -*- Mode: LISP -*-

(defsystem wt.component
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.html
               :alexandria)
  :serial t
  :components ((:module "component"
                        :serial t
                        :components ((:file "package")
                                     (:file "expand")
                                     (:file "render")
                                     (:file "component")
                                     (:file "text")
                                     (:file "icon")
                                     (:file "button")))))
