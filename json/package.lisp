(in-package :cl-user)

(defpackage :json
  (:nicknames :wt.json)
  (:use :cl)
  (:shadow :get)
  (:export :encode
           :decode
           :object
           :alist-object
           :plist-object
           :get
           :do-object
           :json-syntax-error))
