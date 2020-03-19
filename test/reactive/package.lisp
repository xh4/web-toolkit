(in-package :cl-user)

(defpackage :reactive-test
  (:nicknames :wt.reactive-test)
  (:use :cl :reactive :test :alexandria)
  (:shadow :variable)
  (:export :run!))

(in-package :reactive-test)
(def-suite :reactive-test)
