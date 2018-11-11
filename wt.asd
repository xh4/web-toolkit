(in-package :cl-user)
(defpackage wt.asd
  (:use :cl :asdf))
(in-package wt.asd)

(defsystem wt
    :version "0.0.0"
    :author "Xiangyu He"
    :mailto "hexiangyu@coobii.com"
    :defsystem-depends-on (protobuf)
    :depends-on (:alexandria
                 :serapeum
                 :optima.ppcre
                 :quri
                 :rfc2388
                 :log4cl
                 :routes
                 :metabang-bind
                 :closer-mop
                 :cl-json
                 :cl-arrows
                 :local-time
                 :cl-change-case
                 :lparallel
                 :usocket
                 :protobuf
                 :flexi-streams
                 :com.gigamonkeys.binary-data)
    :serial t
    :components ((:module "proto"
                          :components ((:protobuf-source-file "wt_proto_http")))
                 (:file "package")
                 (:file "request")
                 (:file "response")

                 ;; Foundation
                 (:file "handler")
                 (:file "router")
                 (:file "listener")

                 ;; HTML Generation
                 (:file "html-escape")
                 (:file "html-elements")
                 (:file "html")
                 (:file "form-builder")
                 (:file "list-builder")))

(loop for p in (directory (merge-pathnames "vendor/*.*" (component-pathname (find-system :wt))))
   do
     (when (or (null (pathname-name p)) (eq :unspecific (pathname-name p)))
       (pushnew p asdf:*central-registry* :test 'equal)))
