(in-package :cl-user)

(defpackage :svg-test
  (:nicknames :wt.svg-test)
  (:use :cl :svg :test)
  (:shadow :use :symbol)
  (:export :run!))

(in-package :svg-test)
(def-suite :svg-test)
