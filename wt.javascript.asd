;;;; -*- Mode: LISP -*-

(defsystem wt.javascript
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:alexandria
               :cl-ppcre
               :closer-mop)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "javascript"
                :serial t
                :components ((:file "package")
                             (:file "utility")
                             (:file "estree")
                             (:file "character")
                             (:file "scanner")
                             (:file "tokenizer")
                             (:file "parser")
                             (:file "serializer"))))
  :in-order-to ((test-op (test-op :wt.javascript/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :javascript hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.javascript/test
  :depends-on (:wt.javascript
               :wt.http
               :wt.test)
  :components ((:module "test/javascript"
                :serial t
                :components ((:file "package")
                             (:file "helper")
                             (:file "vendor"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :javascript-test)))
