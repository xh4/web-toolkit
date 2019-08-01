(in-package :cl-user)

(defpackage :http
  (:nicknames :wt.http)
  (:use :cl :alexandria)
  (:export :header
           :header-fields
           :header-field
           :field-name
           :field-value
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
           :response-status
           :response-header
           :response-body
           :define-handler
           :handle
           :next-handler
           :call-next-handler
           :listener
           :listener-port
           :listener-address
           :define-server
           :start-server
           :stop-server)
  (:import-from :closer-mop
                :compute-class-precedence-list
                :subclassp)
  (:import-from :cl-change-case
                :header-case))
