;;;; -*- Mode: LISP -*-

(defsystem wt.json
  :version "0.9.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria)
  :serial t
  :components ((:module "json"
                        :serial t
                        :components ((:file "package")
                                     (:file "camel-case")
                                     (:file "common")
                                     (:file "object")
                                     (:file "access")
                                     (:file "encode")
                                     (:file "decode"))))
  :in-order-to ((test-op (test-op :wt.json/test))))

(defsystem wt.json/test
  :depends-on (:wt.json
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "json"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "decode")
                                                           (:file "encode")
                                                           (:file "object"))))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :json-test)))
