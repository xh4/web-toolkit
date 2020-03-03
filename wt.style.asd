;;;; -*- Mode: LISP -*-

(defsystem wt.style
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.uri
               :alexandria
               :closer-mop
               :cl-ppcre)
  :components ((:module "style"
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
                                     (:file "style"))))
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
                                     (:file "helper")
                                     (:file "dimension")
                                     (:file "declaration")
                                     (:file "style"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :style-test)))