(in-package :cl-user)

(defpackage :dom-test
  (:nicknames :wt.dom-test)
  (:use :cl :dom :test)
  (:export :run!))

(in-package :dom-test)

(def-suite :dom-test)
