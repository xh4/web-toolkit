(in-package :cl-user)

(defpackage :javascript
  (:nicknames :js :wt.javascript :wt.js)
  (:use :cl :alexandria)
  (:shadow :position :function :class :keyword :declaration :method)
  (:export
   ;; syntax
   :node
   ;; tokenize
   :tokenize
   ;; parse
   :parse
   ;; serialize
   :serialize)
  (:import-from :closer-mop
                :class-slots
                :slot-definition-name))
