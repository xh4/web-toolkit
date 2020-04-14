(in-package :cl-user)

(defpackage :javascript
  (:nicknames :js :wt.javascript :wt.js)
  (:use :cl :alexandria)
  (:shadow :position :function :class)
  (:export
   ;; token
   :token :token-type :token-value
   ;; scanner
   :scanner))
