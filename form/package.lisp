(in-package :cl-user)

(defpackage :form
  (:nicknames :wt.form)
  (:use :cl :alexandria)
  (:export :define-form
           :text-field
           :email-field
           :password-field)
  (:import-from :component
                :define-component
                :define-render
                :render
                :children)
  (:import-from :http
                :request
                :request-uri
                :request-method
                :request-header
                :request-body
                :*response*
                :response-status
                :response-body
                :header-field
                :header-field-value
                :define-handler
                :call-next-handler
                :handle
                :routing-rule
                :build-routing-rule)
  (:import-from :trivia
                :match))
