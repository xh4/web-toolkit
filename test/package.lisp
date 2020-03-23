(in-package :cl-user)

(defpackage :test
  (:nicknames :wt.test)
  (:use :cl :fiveam)
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
           :compile-and-load-toplevel-forms
           :find-port)
  (:import-from :utility
                :map-tree))
