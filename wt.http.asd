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
               :usocket
               :chunga
               :cl-cont
               #+sbcl
               :sb-introspect)
  :serial t
  :components ((:module "http"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "stream")
                                     (:file "header-field")
                                     (:file "header-fields")
                                     (:file "header")
                                     (:file "mime")
                                     (:file "body")
                                     (:file "message")
                                     (:file "request")
                                     (:file "status")
                                     (:file "version")
                                     (:file "response")
                                     (:file "entity")
                                     (:file "entity-text")
                                     (:file "entity-file")
                                     (:file "entity-html")
                                     (:file "entity-json")
                                     (:file "reply")
                                     (:file "redirect")
                                     (:file "handler")
                                     (:file "handler-default")
                                     (:file "handler-error")
                                     (:file "router")
                                     (:file "static")
                                     (:file "websocket")
                                     (:file "connection")
                                     (:file "listener")
                                     (:file "server")
                                     (:file "client"))))
  :in-order-to ((test-op (test-op :wt.http/test))))

(defsystem wt.http/test
  :depends-on (:wt.http
               :babel-streams
               :fiveam)
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
                                                           (:file "handler")
                                                           (:file "connection")
                                                           (:file "redirect")
                                                           (:file "static"))))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :http-test)))
