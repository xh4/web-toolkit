(in-package :cl-user)
(defpackage wt.asd
  (:use :cl :asdf))
(in-package wt.asd)

(defsystem wt
    :version "0.0.0"
    :author "Xiangyu He"
    :mailto "xh@coobii.com"
    :depends-on (:alexandria
                 :serapeum
                 :optima.ppcre
                 :quri
                 :log4cl
                 :routes
                 :metabang-bind
                 :ng
                 :closer-mop
                 :cl-json
                 :cl-arrows
                 :local-time
                 :cl-change-case
                 :flexi-streams)
    :serial t
    :components ((:file "package")

                 ;; Foundation
                 (:file "request")
                 (:file "response")
                 (:file "router")
                 (:file "handler")

                 ;; NG
                 (:file "ng-integration")

                 ;; HTML Generation
                 (:file "html-escape")
                 (:file "html-elements")
                 (:file "html")

                 ;; From
                 (:file "form-builder")

                 ;; List
                 (:file "list-builder")))

(loop for p in (directory (merge-pathnames "vendor/*.*" (component-pathname (find-system :wt))))
   do
     (when (or (null (pathname-name p)) (eq :unspecific (pathname-name p)))
       (pushnew p asdf:*central-registry* :test 'equal)))
