;;;; -*- Mode: LISP -*-

(defsystem wt.websocket
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.http
               :wt.uri
               :alexandria
               :ironclad
               :bordeaux-threads
               :cl-cont
               :closer-mop
               :usocket
               #+sbcl
               :sb-introspect
               :cl-base64)
  :serial t
  :components ((:module "websocket"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "handler")
                                     (:file "session")
                                     (:file "endpoint")
                                     (:file "frame")
                                     (:file "connection")
                                     (:file "protocol")
                                     (:file "client"))))
  :in-order-to ((test-op (test-op :wt.websocket/test))))

(defsystem wt.websocket/test
  :depends-on (:wt.websocket
               :wt.json
               :fiveam
               :cl-ppcre
               :split-sequence
               :serapeum)
  :serial t
  :components ((:module "test"
                        :components ((:module "websocket"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "autobahn")
                                                           (:file "endpoint")
                                                           (:file "session")
                                                           (:file "websocket"))))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :websocket-test)))
