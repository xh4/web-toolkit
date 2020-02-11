(in-package :cl-user)

(defpackage :uri-test
  (:nicknames :wt.uri-test)
  (:use :cl :uri :test)
  (:export :run!))

(in-package :uri-test)
(def-suite :uri-test)
