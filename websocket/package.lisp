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
           :*session*
           :in-session
           :session-open-p
           :on-message
           :send-text
           :send-binary
           :ping)
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
                :*response*))
