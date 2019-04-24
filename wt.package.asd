;;;; -*- Mode: LISP -*-

(defsystem wt.package
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.json
               :alexandria
               :drakma)
  :serial t
  :components ((:module "package"
                        :serial t
                        :components ((:file "package0")
                                     (:file "registry")
                                     (:file "human")
                                     (:file "script")
                                     (:file "tag")
                                     (:file "distribution")
                                     (:file "repository")
                                     (:file "package")))))
