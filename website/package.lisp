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
                :html :head :meta :body
                :title :div :h1 :link :script)
  (:import-from :component
                :render)
  (:import-from :bootstrap
                :navbar
                :navbar-brand))
