(in-package :cl-user)

(defpackage :http
  (:nicknames :wt.http)
  (:use :cl :alexandria)
  (:shadow :get :delete :read-line :read-char)
  (:export :header
           :header-fields
           :header-field
           :header-field-name
           :header-field-value
           :find-header-field
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
           :reply
           :reply-object
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
           :get
           :put
           :post
           :delete
           :head)
  (:import-from :uri
                :uri
                :uri-scheme
                :uri-host
                :uri-path
                :uri-query
                :uri-string)
  (:import-from :closer-mop
                :compute-class-precedence-list
                :subclassp)
  (:import-from :cl-change-case
                :header-case)
  (:import-from :split-sequence
                :split-sequence))
