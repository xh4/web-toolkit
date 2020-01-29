(in-package :cl-user)

(defpackage :websocket
  (:nicknames :ws :wt.ws :wt.websocket)
  (:use :cl :alexandria)
  (:export :define-endpoint

           :define-session
           :session-opening-uri
           :session-opening-header
           :session-open-p
           :send-text
           :send-binary
           :ping
           :close-session

           :define-handler-macro)
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
  (:import-from :closer-mop
                :compute-class-precedence-list
                :shared-initialize)
  (:import-from :cl-cont
                :call/cc
                :lambda/cc))
