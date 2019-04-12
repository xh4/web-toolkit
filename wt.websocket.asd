;;;; -*- Mode: LISP -*-

(defsystem wt.websocket
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on ()
  :serial t
  :components ((:module "websocket"
                        :serial t
                        :components ((:file "package")
                                     ))))
