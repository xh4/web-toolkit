(in-package :cl-user)

(defpackage :utility-test
  (:nicknames :wt.utility-test)
  (:use :cl :utility :test :alexandria)
  (:export :run!))

(in-package :utility-test)
(def-suite :utility-test)
