(in-package :cl-user)

(defpackage :json
  (:nicknames :wt.json)
  (:use :cl)
  (:shadow :get)
  #+sb-package-locks
  (:lock t)
  (:export :encode
           :decode
           :object
           :alist-object
           :plist-object
           :get
           :do-object
           :json-syntax-error))
