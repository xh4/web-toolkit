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
               :bordeaux-threads
               :trivial-backtrace)
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

(defsystem wt.websocket/test
  :depends-on (:wt.websocket
               :fiveam)
  :serial t
  :components ((:module "test"
                        :components ((:module "websocket"
                                              :components ((:file "package")
                                                           (:file "websocket")))))))
