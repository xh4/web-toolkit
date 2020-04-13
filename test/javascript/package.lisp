(in-package :cl-user)

(defpackage :javascript-test
  (:nicknames :js-test :wt.js-test :wt.javascript-test)
  (:use :cl :javascript :test)
  (:export :run!))

(in-package :javascript-test)
(def-suite :javascript-test)
