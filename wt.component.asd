;;;; -*- Mode: LISP -*-

(defsystem wt.component
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.html
               :wt.style
               :wt.http
               :wt.websocket
               :wt.utility
               :alexandria
               :group-by
               :split-sequence)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "component"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "component")
                                     (:file "style")
                                     (:file "page"))))
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
                                     (:file "render"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :component-test)))
