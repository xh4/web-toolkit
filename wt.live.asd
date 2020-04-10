;;;; -*- Mode: LISP -*-

(defsystem wt.live
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:wt.http
               :wt.websocket
               :wt.html
               :wt.json
               :wt.component
               :wt.reactive
               :group-by
               :parenscript)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "live"
                        :serial t
                        :components ((:file "package")
                                     (:file "page"))))
  :in-order-to ((test-op (test-op :wt.live/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :live hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.live/test
  :depends-on (:wt.test)
  :components ((:module "test/live"
                        :serial t
                        :components ((:file "package"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :live-test)))
