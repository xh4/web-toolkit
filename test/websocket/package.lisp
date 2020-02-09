(in-package :cl-user)

(defpackage :websocket-test
  (:nicknames :wt.websocket-test :ws-test :wt.ws-test)
  (:use :cl :websocket :fiveam :alexandria)
  (:shadow :test-case)
  (:export :run!)
  (:import-from :split-sequence
                :split-sequence))

(in-package :websocket-test)
(def-suite :websocket-test)
