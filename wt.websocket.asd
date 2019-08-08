;;;; -*- Mode: LISP -*-

(defsystem wt.websocket
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.http
               :alexandria
               :cl-ppcre
               :ironclad
               :quri
               ;; :clack
               ;; :hunchentoot
               ;; :websocket-driver-server
               :bordeaux-threads)
  :serial t
  :components ((:module "websocket"
                        :serial t
                        :components ((:file "package")
                                     (:file "session")
                                     (:file "endpoint")
                                     (:file "frame")
                                     (:file "protocol")
                                     (:file "connection")
                                     (:file "message")
                                     ;; (:file "server")
                                     ;; (:file "client")
                                     ))))
