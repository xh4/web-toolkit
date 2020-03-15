(in-package :cl-user)

(defpackage :css-test
  (:nicknames :wt.css-test)
  (:use :cl :css :test)
  (:shadow :length :float :declaration :rem :time :position)
  (:export :run!)
  (:import-from :utility
                :parse))

(in-package :css-test)
(def-suite :css-test)
