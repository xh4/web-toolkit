(in-package :cl-user)

(defpackage :json-test
  (:nicknames :wt.json-test)
  (:use :cl :json :test)
  (:shadow :get)
  (:export :run!))

(in-package :json-test)
(def-suite :json-test)
