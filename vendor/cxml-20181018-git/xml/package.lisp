;;;; package.lisp -- Paketdefinition
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.

(in-package :cl-user)

(defpackage :cxml
  (:use :cl :runes :runes-encoding #-scl :trivial-gray-streams)
  (:export
   ;; xstreams
   #:make-xstream
   #:make-rod-xstream
   #:close-xstream
   #:read-rune
   #:peek-rune
   #:unread-rune
   #:fread-rune
   #:fpeek-rune
   #:xstream-position
   #:xstream-line-number
   #:xstream-column-number
   #:xstream-plist
   #:xstream-encoding
   
   ;; xstream controller protocol
   #:read-octects
   #:xstream/close

   #:attribute-namespace-uri
   #:attribute-local-name
   #:attribute-qname
   #:attribute-value
   
   #:parse
   #:parse-file
   #:parse-stream
   #:parse-rod
   #:parse-octets
   #:parse-empty-document

   #:make-octet-vector-sink
   #:make-octet-stream-sink
   #:make-rod-sink
   #+rune-is-character #:make-string-sink
   #+rune-is-character #:make-character-stream-sink
   ;; See comment in runes/package.lisp
   ;; #-rune-is-character
   #:make-string-sink/utf8
   ;; #-rune-is-character
   #:make-character-stream-sink/utf8

   #:sink-encoding
   #:sink-omit-xml-declaration-p

   #:with-xml-output
   #:with-output-sink
   #:with-namespace
   #:with-element
   #:with-element*
   #:attribute
   #:attribute*
   #:unparse-attribute
   #:cdata
   #:text
   #:doctype
   #:processing-instruction
   #:comment
   #:unescaped

   #:xml-parse-error
   #:well-formedness-violation
   #:validity-error

   #:parse-dtd-file
   #:parse-dtd-stream
   #:make-validator

   #:*cache-all-dtds*
   #:*dtd-cache*
   #:getdtd
   #:remdtd
   #:make-dtd-cache
   #:clear-dtd-cache
   #:make-extid

   #:*catalog*
   #:*prefer*
   #:make-catalog
   #:resolve-uri
   #:resolve-extid
   
   #:make-recoder
   #:make-namespace-normalizer
   #:make-whitespace-normalizer
   #:rod-to-utf8-string
   #:utf8-string-to-rod

   #:broadcast-handler
   #:broadcast-handler-handlers
   #:make-broadcast-handler
   #:sax-proxy
   #:proxy-chained-handler

   #:make-source))
