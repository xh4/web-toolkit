(in-package :cl-user)

(defpackage :http
  (:nicknames :wt.http)
  (:use :cl :alexandria)
  (:shadow :get :delete :read-line :read-char :stream :directory)
  (:export :header
           :header-fields
           :header-field
           :header-field-name
           :header-field-value
           :find-header-field
           :set-header-field
           :request
           :request-method
           :request-uri
           :request-version
           :request-header
           :request-body
           :status
           :status-keyword
           :status-code
           :status-reason-phrase
           :response
           :response-status
           :response-header
           :response-body
           :reply
           :reply-object
           :define-handler
           :*request*
           :*response*
           :call-next-handler
           :abort-handler
           :redirect
           :listener
           :listener-port
           :listener-address
           :router
           :route
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
                :uri-port
                :uri-path
                :uri-query
                :uri-string)
  (:import-from :closer-mop
                :compute-class-precedence-list
                :subclassp
                :validate-superclass)
  (:import-from :cl-change-case
                :header-case)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :trivial-gray-streams
                :fundamental-binary-input-stream
                :fundamental-binary-output-stream)
  (:import-from :cl-cont
                :lambda/cc))
