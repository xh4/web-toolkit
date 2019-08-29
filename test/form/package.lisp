(in-package :cl-user)

(defpackage :form-test
  (:nicknames :wt.form-test)
  (:use :cl :form :fiveam)
  (:export :run!))

(in-package :form-test)
(def-suite :form-test)
