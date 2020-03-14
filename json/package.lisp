(in-package :cl-user)

(defpackage :json
  (:nicknames :wt.json)
  (:use :cl :alexandria)
  (:shadow :null :array :get)
  #+sb-package-locks
  (:lock t)
  (:export
   ;; value
   :value
   ;; null
   :null
   ;; array
   :array
   ;; object
   :object
   :do-object
   ;; access
   :get
   ;; encode
   :encode
   ;; decode
   :decode
   ;; condition
   :json-syntax-error))
