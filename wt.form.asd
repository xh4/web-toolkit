;;;; -*- Mode: LISP -*-

(defsystem wt.form
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.component
               :wt.http
               :alexandria
               :trivia.ppcre
               :cl-change-case
               :quri)
  :serial t
  :components ((:module "form"
                        :serial t
                        :components ((:file "package")
                                     (:file "definition")
                                     (:file "label")
                                     (:file "control")
                                     (:file "input")
                                     (:file "validate")
                                     (:file "field")
                                     (:file "fields")
                                     (:file "file")
                                     (:file "avatar")
                                     (:file "form")
                                     (:file "handler")))))

(defsystem wt.form/test
  :depends-on (:wt.form
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "form"
                                              :components ((:file "package")
                                                           (:file "form")))))))
