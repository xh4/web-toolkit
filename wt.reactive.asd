;;;; -*- Mode: LISP -*-

(defsystem wt.reactive
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:alexandria)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "reactive"
                        :serial t
                        :components ((:file "package")
                                     )))
  :in-order-to ((test-op (test-op :wt.reactive/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :reactive hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.reactive/test
  :depends-on (:wt.test)
  :components ((:module "test/reactive"
                        :serial t
                        :components ((:file "package")
                                     )))
  :perform (test-op (o c)
                    (symbol-call :test :run! :reactive-test)))
