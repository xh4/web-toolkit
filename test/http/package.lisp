(in-package :cl-user)

(defpackage :http-test
  (:nicknames :wt.http-test)
  (:use :cl :http :nst)
  (:shadow :get :delete))
