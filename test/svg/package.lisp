(in-package :cl-user)

(defpackage :svg-test
  (:nicknames :wt.svg-test)
  (:use :cl :svg :test)
  (:export :run!))

(in-package :svg-test)
(def-suite :svg-test)
