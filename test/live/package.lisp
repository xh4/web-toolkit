(in-package :cl-user)

(defpackage :live-test
  (:nicknames :wt.live-test)
  (:use :cl :live :test :alexandria)
  (:export :run!))

(in-package :live-test)
(def-suite :live-test)
