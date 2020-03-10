;;;; -*- Mode: LISP -*-

(defsystem wt.css
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:wt.uri
               :alexandria
               :closer-mop
               :cl-ppcre)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "css"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "dimension")
                                     (:file "declaration")
                                     (:file "percentage")
                                     (:file "length")
                                     (:file "angle")
                                     (:file "time")
                                     (:file "frequency")
                                     (:file "resolution")
                                     (:file "color")
                                     (:file "image")
                                     (:file "font")
                                     (:file "box")
                                     (:file "sizing")
                                     (:file "overflow")
                                     (:file "text")
                                     (:file "flexbox")
                                     (:file "background")
                                     (:file "display")
                                     (:file "position")
                                     (:file "logical")
                                     (:file "rule")
                                     (:file "serialize")
                                     (:file "style"))))
  :in-order-to ((test-op (test-op :wt.css/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :css hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.css/test
  :depends-on (:wt.css
               :wt.test)
  :components ((:module "test/css"
                        :serial t
                        :components ((:file "package")
                                     (:file "helper")
                                     (:file "dimension")
                                     (:file "declaration")
                                     (:file "box")
                                     (:file "style"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :css-test)))
