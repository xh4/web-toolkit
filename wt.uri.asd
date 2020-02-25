;;;; -*- Mode: LISP -*-

(defsystem wt.uri
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :babel
               :maxpc
               :split-sequence)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "uri"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "condition")
                                     (:file "check")
                                     (:file "class")
                                     (:file "parser")
                                     (:file "primitive")
                                     (:file "parse")
                                     (:file "resolve")
                                     (:file "render")
                                     (:file "uri")
                                     (:file "accessor")
                                     (:file "merge")
                                     (:file "query"))))
  :in-order-to ((test-op (test-op :wt.uri/test))))

(defsystem wt.uri/test
  :depends-on (:wt.uri
               :wt.test)
  :components ((:module "test/uri"
                        :serial t
                        :components ((:file "package")
                                     (:file "decode")
                                     (:file "encode")
                                     (:file "check")
                                     (:file "parse")
                                     (:file "query")
                                     (:file "construct")
                                     (:file "update")
                                     (:file "merge")
                                     (:file "render")
                                     (:file "uri"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :uri-test)))
