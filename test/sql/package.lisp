(in-package :cl-user)

(defpackage :sql-test
  (:nicknames :wt.sql-test)
  (:use :cl :sql :fiveam)
  (:export :run!))

(in-package :sql-test)
(def-suite :sql-test)
