;;;; -*- Mode: LISP -*-

(defsystem wt.component
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.html
               :wt.utility
               :alexandria
               :closer-mop
               :group-by
               :split-sequence)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "component"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "component")
                                     (:file "variable"))))
  :in-order-to ((test-op (test-op :wt.component/test))))

(defsystem wt.component/test
  :depends-on (:wt.component
               :wt.test)
  :components ((:module "test/component"
                        :serial t
                        :components ((:file "package")
                                     (:file "helper")
                                     (:file "component")
                                     (:file "render")
                                     (:file "variable"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :component-test)))
