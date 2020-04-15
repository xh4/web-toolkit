(in-package :cl-user)

(defpackage :javascript
  (:nicknames :js :wt.javascript :wt.js)
  (:use :cl :alexandria)
  (:shadow :position :function :class)
  (:export
   ;; token
   :token :token-type :token-value
   ;; tokenize
   :tokenize)
  (:import-from :closer-mop
                :class-direct-slots
                :slot-definition-name))
