(in-package :cl-user)

(defpackage :component-test
  (:nicknames :com-test :wt.component-test :wt.com-test)
  (:use :cl :component :test :alexandria)
  (:shadow :variable)
  (:export :run!)
  (:import-from :html
                :div
                :h1 :h2 :h3 :h4
                :text))

(in-package :component-test)
(def-suite :component-test)
