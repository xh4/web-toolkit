(in-package :cl-user)

(defpackage :json-test
  (:nicknames :wt.json-test)
  (:use :cl :json :fiveam)
  (:shadow :get)
  (:export :run!))

(in-package :json-test)
(def-suite :json-test)
