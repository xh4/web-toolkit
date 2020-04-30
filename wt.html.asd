;;;; -*- Mode: LISP -*-

(defsystem wt.html
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:wt.dom
               :alexandria
               :plump)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "html"
                :serial t
                :components ((:file "package")
                             (:file "condition")
                             (:file "construct")
                             (:file "document")
                             (:file "text")
                             (:file "element")
                             (:file "serialize")
                             (:file "parse"))))
  :in-order-to ((test-op (test-op :wt.html/test)))
  :perform (load-op :after (o c)
             #+lispworks
             (pushnew :html hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.html/test
  :depends-on (:wt.html
               :wt.test)
  :components ((:module "test/html"
                :serial t
                :components ((:file "package")
                             (:file "text")
                             (:file "element")
                             (:file "document")
                             (:file "serialize"))))
  :perform (test-op (o c)
             (symbol-call :test :run! :html-test)))
