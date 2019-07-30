;;;; -*- Mode: LISP -*-

(defsystem wt.http
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:hunchentoot)
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
                                     (:file "listener")
                                     (:file "handler")
                                     (:file "router")
                                     (:file "server")))))
