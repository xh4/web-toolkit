(in-package :cl-user)

(defpackage :http-test
  (:nicknames :wt.http-test)
  (:use :cl :http :uri :test :alexandria)
  (:shadow :get :delete :with-output-to-string)
  (:export :run!))

(in-package :http-test)
(def-suite :http-test)
