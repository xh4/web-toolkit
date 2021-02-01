;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-user)

(defpackage #:cl-uri-templates
  (:use #:common-lisp #:cl-ppcre)
  (:export
   ;; common
   #:uri-template

   ;; exceptions
   #:invalid-uri-warning
   #:invalid-expansion-error
   #:invalid-op-error
   #:invalid-op-vars-error
   #:invalid-arg-error
   #:invalid-var-error

   ;; operators
   #:define-operator

   ;; environment
   #:*uri-environment*
   #:with-uri-environment
   #:uri-var
   #:with-uri-variables
   #:uri-variables-setq

   ;; parsing
   #:enable-uri-template-syntax
   #:read-uri-template
   #:parse-uri-template

   ;; interpolation
   #:expand-uri-template
   #:*encode-uri-string*
   
   ;; destructuring
   #:uri-var
   #:destructure-uri
   #:with-destructured-uri

   #:*decode-uri-string*
   #:uri-template-bind
   #:bind-standard-uri-components

   #:%uri-scheme
   #:%uri-authority
   #:%uri-path
   #:%uri-query
   #:%uri-fragment
   #:%uri-head
   #:%uri-tail
   #:%uri-user
   #:%uri-host
   #:%uri-port
   #:%uri-directory
   #:%uri-file))


(defpackage #:cl-uri-templates.operators
  (:use #:cl)
  (:export
   #:-opt
   #:-neg
   #:-prefix
   #:-suffix
   #:-join
   #:-list))
