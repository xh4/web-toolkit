;;;; -*- Mode: LISP -*-

(defsystem wt.component
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:wt.html
               :wt.css
               :wt.utility
               :wt.reactive
               :alexandria
               :group-by
               :split-sequence)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "component"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "component")
                                     (:file "style"))))
  :in-order-to ((test-op (test-op :wt.component/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :component hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.component/test
  :depends-on (:wt.component
               :wt.test)
  :components ((:module "test/component"
                        :serial t
                        :components ((:file "package")
                                     (:file "helper")
                                     (:file "component")
                                     (:file "render")
                                     (:file "reactive"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :component-test)))
