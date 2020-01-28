(in-package :cl-user)

(defpackage :websocket
  (:nicknames :ws :wt.ws :wt.websocket)
  (:use :cl :alexandria)
  (:export :define-endpoint

           :define-session

           :*session*
           :session-opening-uri
           :session-opening-header
           :session-open-p
           :send-text
           :send-binary
           :ping
           :close-session

           :define-session-pool
           :session-pool
           :add-session
           :remove-session
           :session-pool-sessions

           :define-handler-macro
           :*handler-macros*)
  (:import-from :http
                :define-handler
                :handler
                :handle
                :call-next-handler
                :request
                :request-uri
                :request-header
                :header
                :header-field
                :header-fields
                :header-field-name
                :header-field-value
                :find-header-field
                :reply
                :status
                :*response*)
  (:import-from :cl-cont
                :call/cc
                :lambda/cc))
