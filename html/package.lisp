(in-package :cl-user)

(defpackage :html
  (:nicknames :wt.html)
  (:use :cl :alexandria)
  (:export :html
           :html-string
           :*html-indent-size*
           :element-form-p
           :segment-element-form)
  (:import-from :serapeum
                :escape))
