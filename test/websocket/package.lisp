(in-package :cl-user)

(defpackage :websocket-test
  (:nicknames :wt.websocket-test :ws-test :wt.ws-test)
  (:use :cl :websocket :test :alexandria)
  (:shadow :test-case)
  (:export :run!)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :utility
                :walk-tree))

(in-package :websocket-test)
(def-suite :websocket-test)
