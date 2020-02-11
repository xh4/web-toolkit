;;; -*- Mode: Lisp; readtable: runes; -*-
;;;  (c) copyright 2007 David Lichteblau

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(cl:defpackage #:klacks
  (:use)
  (:export #:source
	   #:close-source
	   #:with-open-source

	   #:tapping-source
	   #:make-tapping-source
	   #:dribble-handler

	   #:peek
	   #:peek-value
	   #:peek-next
	   #:consume

	   #:expect
	   #:skip
	   #:find-element
	   #:find-event
	   #:expecting-element

	   #:map-attributes
	   #:list-attributes
	   #:get-attribute
	   #:current-uri
	   #:current-lname
	   #:current-qname
	   #:current-characters
	   #:consume-characters
	   #:current-cdata-section-p
	   #:map-current-namespace-declarations

	   #:serialize-event
	   #:serialize-element
	   #:serialize-source

	   #:klacks-error

	   #:current-line-number
	   #:current-column-number
	   #:current-system-id
	   #:current-xml-base

	   #:find-namespace-binding
	   #:decode-qname))
