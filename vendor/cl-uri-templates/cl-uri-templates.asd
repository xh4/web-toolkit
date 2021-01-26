;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-user)

(defpackage #:cl-uri-templates.system
  (:use #:cl #:asdf))

(in-package #:cl-uri-templates.system)

(defsystem #:cl-uri-templates
  :version "0.6"
  :serial t
  :components ((:file "defpackage")
               (:file "uri")
               (:file "uri-environment")
               (:file "uri-template")
               (:file "expansion")
               (:file "operators")
               (:file "destructure-uri"))
  :depends-on (#:trivial-utf-8 #:cl-ppcre))
