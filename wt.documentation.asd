;;;; -*- Mode: LISP -*-

(defsystem wt.documentation
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:alexandria
               :wt.html
               :wt.json
               :wt.component
               :wt.http
               :wt.live
               :cl-ppcre
               :closer-mop)
  :defsystem-depends-on (:wt.vendor)
  :components ((:module "documentation"
                        :serial t
                        :components ((:file "package")
                                     (:file "utility")
                                     (:file "style")
                                     (:file "evil")
                                     (:file "addressable")
                                     (:file "symbol")
                                     (:file "chapter")
                                     (:file "article")
                                     (:file "chapter-get-started")
                                     (:file "chapter-uri")
                                     (:file "chapter-http")
                                     (:file "chapter-html")
                                     (:file "chapter-json")
                                     (:file "chapter-css")
                                     (:file "chapter-websocket")
                                     (:file "chapter-component")
                                     (:file "chapter-form")
                                     (:file "documentation")
                                     (:file "server")))))
