;;;; -*- Mode: LISP -*-

(defsystem wt.svg
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:wt.dom
               :alexandria)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "svg"
                :serial t
                :components ((:file "package")
                             )))
  :in-order-to ((test-op (test-op :wt.svg/test)))
  :perform (load-op :after (o c)
             #+lispworks
             (pushnew :svg hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.svg/test
  :depends-on (:wt.svg
               :wt.test)
  :components ((:module "test/svg"
                :serial t
                :components ((:file "package")
                             )))
  :perform (test-op (o c)
             (symbol-call :test :run! :svg-test)))
