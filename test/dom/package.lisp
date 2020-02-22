(in-package :cl-user)

(defpackage :dom-test
  (:nicknames :wt.dom-test)
  (:use :cl :alexandria :dom :test :utility)
  (:shadow :node)
  (:export :run!))

(in-package :dom-test)

(def-suite :dom-test)
