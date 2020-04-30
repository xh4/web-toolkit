;;;; -*- Mode: LISP -*-

(defsystem wt.ant-vision
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:alexandria
               :wt.component)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "ant-vision"
                :serial t
                :components ((:file "package")
                             )))
  :in-order-to ((test-op (test-op :wt.ant-vision/test)))
  :perform (load-op :after (o c)
             #+lispworks
             (pushnew :ant-vision hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.ant-vision/test
  :depends-on (:wt.test)
  :components ((:module "test/ant-vision"
                :serial t
                :components ((:file "package")
                             )))
  :perform (test-op (o c)
             (symbol-call :test :run! :ant-vision-test)))
