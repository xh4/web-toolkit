;;;; -*- Mode: LISP -*-

(defsystem wt.vendor
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :components ((:module "vendor"
                :serial t
                :components ((:file "package")
                             #+quicklisp
                             (:file "quicklisp")
                             (:file "vendor"))))
  :perform (load-op :before (o c)
             (symbol-call :vendor :register)))
