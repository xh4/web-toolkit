;;; cl-change-case-test.asd

(in-package :asdf-user)

(defsystem #:cl-change-case-test
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :description "Test system of cl-change-case"
  :license "LLGPL"
  :depends-on (:fiveam
               :cl-change-case)
  :components ((:module "t"
                :components ((:file "cl-change-case"))))
  :perform (test-op (op c)
                    (asdf:clear-system c)
                    (uiop:symbol-call :5am :run! :cl-change-case)))
