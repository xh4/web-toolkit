(in-package :cl-user)

(defpackage :javascript-test
  (:nicknames :js-test :wt.js-test :wt.javascript-test)
  (:use :cl :javascript :estree :test)
  (:shadowing-import-from :estree
   :position :function :class :keyword :declaration :method :block)
  (:shadowing-import-from :fiveam :test)
  (:export :run!)
  (:import-from :closer-mop
                :class-slots
                :slot-definition-name))

(in-package :javascript-test)
(def-suite :javascript-test)
