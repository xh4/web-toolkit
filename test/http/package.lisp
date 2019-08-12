(in-package :cl-user)

(defpackage :http-test
  (:nicknames :wt.http-test)
  (:use :cl :http :fiveam)
  (:shadow :get :delete)
  (:export :run!))

(in-package :http-test)

(def-suite :http-test)
