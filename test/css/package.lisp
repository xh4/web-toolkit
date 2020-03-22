(in-package :cl-user)

(defpackage :css-test
  (:nicknames :wt.css-test)
  (:use :cl :css :test :alexandria)
  (:shadowing-import-from :css
                          :float
                          :declaration
                          :rem
                          :position
                          :shadow
                          :rotate)
  (:export :run!)
  (:import-from :utility
                :parse))

(in-package :css-test)
(def-suite :css-test)
