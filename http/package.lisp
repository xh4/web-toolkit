(in-package :cl-user)

(defpackage :http
  (:nicknames :wt.http)
  (:use :cl :alexandria)
  (:shadow :get :delete)
  (:export :header
           :header-fields
           :header-field
           :header-field-name
           :header-field-value
           :body
           :request
           :request-method
           :request-uri
           :request-header
           :request-body
           :status
           :status-keyword
           :status-code
           :status-reason-phrase
           :response
           :*response*
           :response-status
           :response-header
           :response-body
           :define-handler
           :handle
           :next-handler
           :call-next-handler
           :redirect
           :listener
           :listener-port
           :listener-address
           :router
           :routing-rule
           :build-routing-rule
           :define-server
           :start-server
           :stop-server
           :put
           :post
           :delete
           :head)
  (:import-from :closer-mop
                :compute-class-precedence-list
                :subclassp)
  (:import-from :cl-change-case
                :header-case)
  (:import-from :split-sequence
                :split-sequence))
