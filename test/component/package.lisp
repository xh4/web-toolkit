(in-package :cl-user)

(defpackage :component-test
  (:nicknames :com-test :wt.component-test :wt.com-test)
  (:use :cl :component :test)
  (:shadow :define-component :define-variable :variable)
  (:export :run!))

(in-package :component-test)
(def-suite :component-test)
