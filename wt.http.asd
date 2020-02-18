;;;; -*- Mode: LISP -*-

(defsystem wt.http
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.uri
               :wt.json
               :wt.html
               :alexandria
               :closer-mop
               :babel
               :cl-change-case
               :split-sequence
               :cl-fad
               :trivial-backtrace
               :trivial-gray-streams
               #-lispworks
               :usocket
               #-lispworks
               :cl+ssl
               :chunga
               :cl-cont
               #+sbcl
               :sb-introspect
               :local-time)
  :defsystem-depends-on (:wt.vendor)
  :serial t
  :components ((:module "http"
                        :serial t
                        :components ((:file "package")
                                     (:file "conditions")
                                     (:file "utility")
                                     (:file "stream")
                                     (:file "header-field")
                                     (:file "header-fields")
                                     (:file "header")
                                     (:file "mime")
                                     (:file "form")
                                     (:file "message")
                                     (:file "request")
                                     (:file "status")
                                     (:file "version")
                                     (:file "response")
                                     (:file "file")
                                     (:file "directory")
                                     (:file "entity")
                                     (:file "entity-text")
                                     (:file "entity-html")
                                     (:file "entity-json")
                                     (:file "entity-file")
                                     (:file "entity-directory")
                                     (:file "entity-form")
                                     (:file "reply")
                                     (:file "redirect")
                                     (:file "handler")
                                     (:file "handler-error")
                                     (:file "handler-application")
                                     (:file "handler-default")
                                     (:file "handler-anonymous")
                                     (:file "router")
                                     (:file "static")
                                     (:file "websocket")
                                     (:file "connection")
                                     (:file "connection-manager")
                                     (:file "listener")
                                     (:file "server")
                                     (:file "client"))))
  :in-order-to ((test-op (test-op :wt.http/test))))

(defsystem wt.http/test
  :depends-on (:wt.http
               :wt.test
               :babel-streams)
  :serial t
  :components ((:module "test"
                        :components ((:module "http"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "helper")
                                                           (:file "stream")
                                                           (:file "utility")
                                                           (:file "status")
                                                           (:file "header-field")
                                                           (:file "header")
                                                           (:file "request")
                                                           (:file "response")
                                                           (:file "entity")
                                                           (:file "entity-text")
                                                           (:file "entity-html")
                                                           (:file "handler")
                                                           (:file "connection")
                                                           (:file "redirect")
                                                           (:file "error")
                                                           (:file "router")
                                                           (:file "file")
                                                           (:file "directory")
                                                           (:file "static")
                                                           (:file "client"))))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :http-test)))
