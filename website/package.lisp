(in-package :cl-user)

(defpackage :website
  (:use :cl)
  (:import-from :http
                :define-server
                :define-handler
                :handle
                :request
                :*response*
                :listener
                :header-field
                :response-status
                :response-header
                :response-body)
  (:import-from :html
                :document
                :html
                :head
                :body
                :title
                :div
                :h1))
