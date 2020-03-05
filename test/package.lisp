(in-package :cl-user)

(defpackage :test
  (:nicknames :wt.test)
  (:use :cl :fiveam :utility)
  (:export :def-suite
           :in-suite
           :test
           :is :is-true
           :is-every :is-false
           :signals :finishes
           :pass :fail
           :run! :run
           :*on-error* :*on-failure*
           :it :*it* :*is* :=>
           :ensure-cleanup
           :compile-and-load-toplevel-forms))
