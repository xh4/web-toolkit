;;; cl-change-case.asd

(in-package :asdf-user)

(defsystem #:cl-change-case
  :version "0.1.0"
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :license "LLGPL"
  :source-control (:git "git@github.com:rudolfochrist/cl-change-case.git")
  :bug-tracker "https://github.com/rudolfochrist/cl-change-case/issues"
  :depends-on (:cl-ppcre
               :cl-ppcre-unicode)
  :components ((:module "src"
                :components ((:file "cl-change-case"))))
  :description "Convert strings between camelCase, param-case, PascalCase and more"
  :in-order-to ((test-op (test-op :cl-change-case-test))))
