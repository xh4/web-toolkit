(in-package :cl-user)

(defpackage :websocket-test
  (:nicknames :wt.websocket-test :ws-test :wt.ws-test)
  (:use :cl :websocket :fiveam)
  (:export :run!))

(in-package :websocket-test)
(def-suite :websocket-test)