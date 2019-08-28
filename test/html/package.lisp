(in-package :cl-user)

(defpackage :html-test
  (:nicknames :wt.html-test)
  (:use :cl :html :fiveam)
  (:shadow :time :map)
  (:export :run!))

(in-package :html-test)
(def-suite :html-test)
