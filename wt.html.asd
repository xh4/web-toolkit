;;;; -*- Mode: LISP -*-

(defsystem wt.html
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :cxml-dom
               :closure-html)
  :serial t
  :components ((:module "html"
                        :serial t
                        :components ((:file "package")
                                     (:file "html")
                                     (:file "serialize"))))
  :in-order-to ((test-op (test-op :wt.html/test))))

(defsystem wt.html/test
  :depends-on (:wt.html
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "html"
                                              :components ((:file "package")
                                                           (:file "text")
                                                           (:file "element")
                                                           (:file "serialize"))))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :html-test)))
