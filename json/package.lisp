(in-package :cl-user)

(defpackage :json
  (:nicknames :wt.json)
  (:use :cl)
  (:export :encode-json
           :decode-json
           :object
           :alist-object))
