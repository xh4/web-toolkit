;;;; -*- Mode: LISP -*-

(defsystem wt.utility
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :cl-cont
               (:feature :sbcl (:require :sb-introspect)))
  :components ((:module "utility"
                        :serial t
                        :components ((:file "package")
                                     (:file "macro")
                                     (:file "function")
                                     (:file "class")
                                     (:file "tree"))))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :utility hcl:*packages-for-warn-on-redefinition*)))
