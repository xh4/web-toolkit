;;;; -*- Mode: LISP -*-

(defsystem wt.dom
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria)
  :serial t
  :components ((:module "dom"
                        :serial t
                        :components ((:file "package")
                                     (:file "mixin")
                                     (:file "node")
                                     (:file "document")
                                     (:file "element")
                                     (:file "text"))))
  :in-order-to ((test-op (test-op :wt.dom/test))))

(defsystem wt.dom/test
  :depends-on (:wt.dom
               :wt.test)
  :serial t
  :components ((:module "test"
                        :components ((:module "dom"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "element"))))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :dom-test)))
