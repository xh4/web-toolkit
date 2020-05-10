(in-package :cl-user)

(defpackage :html-test
  (:nicknames :wt.html-test)
  (:use :cl :html :test)
  (:shadow :time :map :parse-error)
  (:export :run!))

(in-package :html-test)
(def-suite :html-test)
