(in-package :cl-user)

(defpackage :websocket
  (:nicknames :ws :wt.ws :wt.websocket)
  (:use :cl :alexandria)
  (:export :define-endpoint
           :define-session
           :session-class
           :session-opening-uri
           :session-opening-header
           :session-open-p
           :send-text
           :send-binary
           :ping
           :close-session
           :connect)
  (:import-from :http
                :request
                :request-uri
                :request-header
                :request-body
                :header
                :header-field
                :header-fields
                :header-field-name
                :header-field-value
                :find-header-field
                :define-handler
                :reply
                :status
                :*response*
                :response-header)
  (:import-from :uri
                :uri-scheme
                :uri-host
                :uri-port)
  (:import-from :closer-mop
                :compute-class-precedence-list
                :shared-initialize
                :validate-superclass)
  (:import-from :cl-cont
                :call/cc
                :lambda/cc))
