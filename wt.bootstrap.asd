;;;; -*- Mode: LISP -*-

(defsystem wt.bootstrap
  :version "0.0.0"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.component
               :trivia)
  :serial t
  :components ((:module "bootstrap"
                        :serial t
                        :components ((:file "package")
                                     (:File "layout")
                                     (:file "typography")
                                     (:file "code")
                                     (:file "image")
                                     (:file "table")
                                     (:file "figure")
                                     (:file "alert")
                                     (:file "badge")
                                     (:file "breadcrumb")
                                     (:file "button")
                                     (:file "button-group")
                                     (:file "card")
                                     (:file "carousel")
                                     (:file "collapse")
                                     (:file "dropdown")
                                     (:file "form")
                                     (:file "input-group")
                                     (:file "jumbotron")
                                     (:file "list-group")
                                     (:file "media-object")
                                     (:file "modal")
                                     (:file "nav")
                                     (:file "navbar")
                                     (:file "pagination")
                                     (:file "popover")
                                     (:file "progress")
                                     (:file "scrollspy")
                                     (:file "spinner")
                                     (:file "toast")
                                     (:file "tooltip")))))
