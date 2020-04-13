;;;; -*- Mode: LISP -*-

(defsystem wt.javascript
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:alexandria)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "javascript"
                        :serial t
                        :components ((:file "package")
                                     )))
  :in-order-to ((test-op (test-op :wt.javascript/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :javascript hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.javascript/test
  :depends-on (:wt.javascript
               :wt.test)
  :components ((:module "test/javascript"
                        :serial t
                        :components ((:file "package")
                                     )))
  :perform (test-op (o c)
                    (symbol-call :test :run! :javascript-test)))
