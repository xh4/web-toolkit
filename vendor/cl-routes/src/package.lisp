;;;; package.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:routes
  (:use #:cl #:iter #:split-sequence)
  (:export #:+no-bindings+

           #:uri-component-template
           #:variable-template
           #:custom-variable-template
           #:wildcard-template
           #:concat-template
           #:or-template
           #:wildcard-template
           #:template-data
           #:variable-parse-fun

           ;; routes
           #:base-route
           #:route-template
           #:route-name
           #:route-check-conditions

           #:route
           #:make-route
           
           #:proxy-route
           #:proxy-route-target
           
           #:route-variables

           #:parse-template
           #:template-variables

           ;; mapper
           #:mapper
           #:match
           #:connect
           #:reset-mapper))