(in-package :cl-user)

(defpackage :websocket
  (:nicknames :ws :wt.ws :wt.websocket)
  (:use :cl :alexandria)
  (:export :define-endpoint
           :on-open
           :on-close
           :on-error

           :define-session
           :close-session
           :on-message
           :send-text
           :send-binary
           :ping)
  (:import-from :http
                :define-handler
                :handler
                :handle
                :request
                :request-uri
                :request-header
                :header-field
                :header-fields
                :header-field-name
                :header-field-value
                :response-status
                :response-body
                :*response*))
