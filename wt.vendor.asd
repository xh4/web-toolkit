;;;; -*- Mode: LISP -*-

(defsystem wt.vendor
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :serial t
  :components ((:module "vendor"
                        :serial t
                        :components ((:file "package")
                                     (:file "vendor"))))
  :perform (load-op :before (o c)
                    (symbol-call :vendor :ensure-vendors)))
