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
               :bordeaux-threads)
  :serial t
  :components ((:module "websocket"
                        :serial t
                        :components ((:file "package")
                                     (:file "session")
                                     (:file "endpoint")
                                     (:file "frame")
                                     (:file "connection")
                                     (:file "protocol")
                                     (:file "message")
                                     ;; (:file "server")
                                     ;; (:file "client")
                                     ))))
