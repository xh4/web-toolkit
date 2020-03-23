;;;; -*- Mode: LISP -*-

(defsystem wt.http
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :license "BSD 3-Clause"
  :depends-on (:wt.uri
               :wt.json
               :wt.html
               :wt.utility
               :alexandria
               :closer-mop
               :babel
               :babel-streams
               :cl-change-case
               :split-sequence
               :cl-fad
               :trivial-backtrace
               :trivial-gray-streams
               (:feature (:not :lispworks) :usocket)
               (:feature (:not :lispworks) :cl+ssl)
               :chunga
               :cl-cont
               :local-time)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "http"
                        :serial t
                        :components ((:file "package")
                                     (:file "condition")
                                     (:file "utility")
                                     (:file "stream")
                                     (:file "header-field")
                                     (:file "header-fields")
                                     (:file "header")
                                     (:file "mime")
                                     (:file "form")
                                     (:file "multipart")
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
  :in-order-to ((test-op (test-op :wt.http/test)))
  :perform (load-op :after (o c)
                    #+lispworks
                    (pushnew :http hcl:*packages-for-warn-on-redefinition*)))

(defsystem wt.http/test
  :depends-on (:wt.http
               :wt.test
               :babel-streams)
  :components ((:module "test/http"
                        :serial t
                        :components ((:file "package")
                                     (:file "helper")
                                     (:file "stream")
                                     (:file "utility")
                                     (:file "multipart")
                                     (:file "status")
                                     (:file "header-field")
                                     (:file "header")
                                     (:file "request")
                                     (:file "response")
                                     (:file "entity")
                                     (:file "entity-text")
                                     (:file "entity-html")
                                     (:file "entity-form")
                                     (:file "handler")
                                     (:file "connection")
                                     (:file "redirect")
                                     (:file "error")
                                     (:file "router")
                                     (:file "file")
                                     (:file "directory")
                                     (:file "static")
                                     (:file "form")
                                     (:file "client"))))
  :perform (test-op (o c)
                    (symbol-call :test :run! :http-test)))
