(in-package :cl-user)

(defpackage :websocket
  (:nicknames :ws :wt.ws :wt.websocket)
  (:use :cl :alexandria)
  (:export :define-endpoint
           :on-open
           :on-close
           :on-error
           :on-message
           :send-text
           :send-binary
           :send-ping
           :send-pong
           :define-session
           :close-session
           :define-server))