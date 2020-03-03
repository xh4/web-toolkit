(in-package :cl-user)

(defpackage :style-test
  (:nicknames :wt.style-test)
  (:use :cl :style :test)
  (:shadow :rem)
  (:export :run!))

(in-package :style-test)
(def-suite :style-test)
