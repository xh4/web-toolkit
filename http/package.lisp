(in-package :cl-user)

(defpackage :http
  (:nicknames :wt.http)
  (:use :cl :alexandria)
  (:shadow :get :delete :read-line :read-char :stream :directory)
  #+sb-package-locks
  (:lock t)
  (:export
   ;; header-field
   :header-field
   :header-field-name
   :header-field-value
   ;; header
   :header
   :header-fields
   :find-header-field
   :set-header-field
   ;; request
   :request
   :request-method
   :request-uri
   :request-version
   :request-header
   :request-body
   ;; status
   :status
   :status-keyword
   :status-code
   :status-reason-phrase
   ;; response
   :response
   :response-status
   :response-header
   :response-body
   :read-response-body-into-vector
   :read-response-body-into-temporary-file
   ;; entity
   :entity
   ;; entity-html
   :html-entity
   :entity-html
   ;; entity-json
   :json-entity
   :entity-json
   ;; entity-file
   :file-entity
   ;; entity-directory
   :directory-entity
   ;; entity-form
   :form-entity
   :entity-form
   ;; entity-text
   :text-entity
   :entity-text
   ;; reply
   :reply
   :reply-object
   ;; handler
   :define-handler
   :*request*
   :*response*
   :call-next-handler
   :abort-handler
   ;; redirect
   :redirect
   ;; listener
   :listener
   :listener-port
   :listener-address
   ;; router
   :router
   :route
   :make-route
   ;; server
   :define-server
   :start-server
   :stop-server
   ;; client
   :get
   :head
   :put
   :post
   :delete
   :patch
   :options
   :with-connections
   ;; form
   :form
   :form-field
   :form-field-name
   :form-field-value
   :form-fields
   :skip-form-field
   :read-form-field-into-vector
   :read-form-field-into-stream
   :read-form-field-into-file
   :read-form-field-into-temporary-file)
  (:import-from :utility
                :function-lambda-list
                :rewrite-class-option)
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
                :lambda/cc
                :funcallable/cc))
