;;;; -*- Mode: LISP -*-

(defsystem wt.websocket
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :clack
               :hunchentoot
               :websocket-driver-server)
  :serial t
  :components ((:module "websocket"
                        :serial t
                        :components ((:file "package")
                                     (:file "session")
                                     (:file "endpoint")
                                     (:file "message")
                                     (:file "server")
                                     (:file "client")))))