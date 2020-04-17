(in-package :cl-user)

(defpackage :javascript
  (:nicknames :js :wt.javascript :wt.js)
  (:use :cl :alexandria)
  (:shadow :position :function :class :keyword)
  (:export
   ;; tokenize
   :tokenize
   ;; parse
   :parse)
  (:import-from :closer-mop
                :class-slots
                :slot-definition-name))
