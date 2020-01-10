;;;; -*- Mode: LISP -*-

(defsystem wt.sql
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               )
  :serial t
  :components ((:module "sql"
                        :serial t
                        :components ((:file "package")
                                     )))
  :in-order-to ((test-op (test-op :wt.sql/test))))

(defsystem wt.sql/test
  :depends-on (:wt.sql
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "sql"
                                              :components ((:file "package")
                                                           )))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :sql-test)))
