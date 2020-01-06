;;;; -*- Mode: LISP -*-

(defsystem wt.uri
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :babel
               :maxpc
               :split-sequence)
  :serial t
  :components ((:module "uri"
                        :serial t
                        :components ((:file "package")
                                     (:file "util")
                                     (:file "error")
                                     (:file "check")
                                     (:file "class")
                                     (:file "parser")
                                     (:file "primitive")
                                     (:file "normalize")
                                     (:file "parse")
                                     (:file "resolve")
                                     (:file "render")
                                     (:file "uri")
                                     (:file "accessor")
                                     (:file "query"))))
  :in-order-to ((test-op (test-op "wt.uri/test"))))

(defsystem wt.uri/test
  :depends-on (:wt.uri
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "uri"
                                              :components ((:file "package")
                                                           (:file "decode")
                                                           (:file "encode")
                                                           (:file "check")
                                                           (:file "parse")
                                                           (:file "query")
                                                           (:file "construct")
                                                           (:file "update")
                                                           (:file "uri"))))))
  :perform (test-op (o c)
                    (symbol-call :fiveam '#:run! :uri-test)))
