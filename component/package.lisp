(in-package :cl-user)

(defpackage :component
  (:nicknames :com :wt.com :wt.component)
  (:use :cl :alexandria :utility)
  (:shadow :variable)
  (:export :id
           :define-component
           :render
           :serialize
           :root
           :children
           :append-child
           :define-variable
           :variable)
  (:import-from :html
                :append-child
                :serialize
                :children)
  (:import-from :style
                :style
                :qualified-rule)
  (:import-from :http
                :route
                :make-route
                :request-method
                :define-handler
                :find-header-field
                :header-field-value
                :reply)
  (:import-from :uri
                :uri-path)
  (:import-from :websocket
                :endpoint
                :define-endpoint
                :define-session)
  (:import-from :closer-mop
                :allocate-instance
                :validate-superclass
                :class-slots
                :slot-definition-name
                :slot-definition-initargs
                :compute-class-precedence-list
                :ensure-finalized)
  (:import-from :split-sequence
                :split-sequence))
