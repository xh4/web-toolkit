(in-package :cl-user)

(defpackage :json
  (:nicknames :wt.json)
  (:use :cl)
  (:shadow :get)
  (:export :encode-json
           :decode-json
           :object
           :alist-object
           :plist-object
           :get
           :do-object))
