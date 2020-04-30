(in-package :cl-user)

(defpackage :printer-test
  (:nicknames :wt.printer-test)
  (:use :cl :printer :test)
  (:export :run!))

(in-package :printer-test)
(def-suite :printer-test)
