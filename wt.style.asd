;;;; -*- Mode: LISP -*-

(defsystem wt.style
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.uri
               :alexandria)
  :components ((:module "style"
                        :serial t
                        :components ((:file "package")
                                     (:file "color")
                                     (:file "syntax"))))
  :in-order-to ((test-op (test-op :wt.style/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :style hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.style/test
  :depends-on (:wt.style
               :wt.test)
  :components ((:module "test/style"
                        :serial t
                        :components ((:file "package")
                                     (:file "style"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :style-test)))
