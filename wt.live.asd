;;;; -*- Mode: LISP -*-

(defsystem wt.live
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.http
               :wt.websocket
               :wt.html)
  :serial t
  :components ((:module "live"
                        :serial t
                        :components ((:file "package")
                                     ))))
