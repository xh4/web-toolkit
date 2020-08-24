;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-user)

(defpackage #:cl-uri-templates.test.system
  (:use #:cl #:asdf))

(in-package #:cl-uri-templates.test.system)

(defsystem #:cl-uri-templates.test
    :components ((:file "test"))
    :depends-on (#:FiveAM #:cl-uri-templates))
