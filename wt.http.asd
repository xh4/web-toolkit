;;;; -*- Mode: LISP -*-

(defsystem wt.http
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:hunchentoot
               :alexandria
               :closer-mop
               :cl-change-case
               :split-sequence
               :cl-fad)
  :serial t
  :components ((:module "http"
                        :serial t
                        :components ((:file "package")
                                     (:file "header")
                                     (:file "body")
                                     (:file "request")
                                     (:file "status")
                                     (:file "version")
                                     (:file "response")
                                     (:file "handler")
                                     (:file "router")
                                     (:file "listener")
                                     (:file "server")))))
