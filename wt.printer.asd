;;;; -*- Mode: LISP -*-

(defsystem wt.printer
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:alexandria)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "printer"
                        :serial t
                        :components ((:file "package")
                                     )))
  :in-order-to ((test-op (test-op :wt.printer/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :printer hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.printer/test
  :depends-on (:wt.printer
               :wt.test)
  :components ((:module "test/printer"
                        :serial t
                        :components ((:file "package")
                                     )))
  :perform (test-op (o c)
                    (symbol-call :test :run! :printer-test)))
