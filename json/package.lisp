(in-package :cl-user)

(defpackage :wt.json
  (:use :cl)
  (:export :encode-json
           :decode-json
           :object))
