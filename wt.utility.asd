;;;; -*- Mode: LISP -*-

(defsystem wt.utility
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :cl-cont
               :closer-mop
               :trivial-garbage
               (:feature :sbcl (:require :sb-introspect)))
  :components ((:module "utility"
                        :serial t
                        :components ((:file "package")
                                     (:file "macro")
                                     (:file "function")
                                     (:file "class")
                                     (:file "tree")
                                     (:file "reflective")
                                     (:file "variable"))))
  :in-order-to ((test-op (test-op :wt.utility/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :utility hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.utility/test
  :depends-on (:wt.utility
               :wt.test)
  :components ((:module "test/utility"
                        :serial t
                        :components ((:file "package")
                                     (:file "helper")
                                     (:file "variable"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :utility-test)))
