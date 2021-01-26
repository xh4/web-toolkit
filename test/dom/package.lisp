(in-package :cl-user)

(defpackage :dom-test
  (:nicknames :wt.dom-test)
  (:use :cl :alexandria :dom :test)
  (:shadow :node)
  (:shadowing-import-from :dom
                          :length :append :remove :class-name)
  (:export :run!))

(in-package :dom-test)
(def-suite :dom-test)
