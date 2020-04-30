;;;; -*- Mode: LISP -*-

(defsystem wt.ant-design
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:alexandria
               :wt.component)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "ant-design"
                :serial t
                :components ((:file "package")
                             (:file "style")
                             (:file "component")
                             (:file "button"))))
  :in-order-to ((test-op (test-op :wt.ant-design/test)))
  :perform (load-op :after (o c)
             #+lispworks
             (pushnew :ant-design hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.ant-design/test
  :depends-on (:wt.test)
  :components ((:module "test/ant-design"
                :serial t
                :components ((:file "package")
                             )))
  :perform (test-op (o c)
             (symbol-call :test :run! :ant-design-test)))
