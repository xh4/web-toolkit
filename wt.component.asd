;;;; -*- Mode: LISP -*-

(defsystem wt.component
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.html
               :alexandria
               :closer-mop
               :group-by)
  :serial t
  :components ((:module "component"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "class")
                                     (:file "component")
                                     (:file "tag-option")
                                     (:file "class-option")
                                     (:file "render")
                                     (:file "element"))))
  :in-order-to ((test-op (test-op :wt.component/test))))

(defsystem wt.component/test
  :depends-on (:wt.component
               :wt.test)
  :serial t
  :components ((:module "test"
                        :components ((:module "component"
                                              :components ((:file "package")
                                                           (:file "component")
                                                           (:file "render"))))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :component-test)))
