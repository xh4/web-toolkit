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

(cl:in-package #:cxml)

(defclass klacks:source ()
    (
     ;; fixme, terrible DTD kludges
     (internal-declarations)
     (external-declarations :initform nil)
     (dom-impl-dtd :initform nil)
     (dom-impl-entity-resolver :initform nil)))

(defgeneric klacks:close-source (source))

(defgeneric klacks:peek (source))
(defgeneric klacks:peek-value (source))
(defgeneric klacks:consume (source))

(defgeneric klacks:map-attributes (fn source))
(defgeneric klacks:list-attributes (source))
(defgeneric klacks:get-attribute (source lname &optional uri))
;;;(defgeneric klacks:current-uri (source))
;;;(defgeneric klacks:current-lname (source))
;;;(defgeneric klacks:current-qname (source))
;;;(defgeneric klacks:current-characters (source))
(defgeneric klacks:current-cdata-section-p (source))
(defgeneric klacks:map-current-namespace-declarations (fn source))

(defgeneric klacks:current-line-number (source))
(defgeneric klacks:current-column-number (source))
(defgeneric klacks:current-system-id (source))
(defgeneric klacks:current-xml-base (source))

(defgeneric klacks:find-namespace-binding (prefix source))
(defgeneric klacks:decode-qname (qname source))

(defmacro klacks:with-open-source ((var source) &body body)
  `(let ((,var ,source))
     (unwind-protect
	 (progn ,@body)
       (klacks:close-source ,var))))

(defun klacks:current-uri (source)
  (multiple-value-bind (key uri lname qname) (klacks:peek source)
    (declare (ignore lname qname))
    (check-type key (member :start-element :end-element))
    uri))

(defun klacks:current-lname (source)
  (multiple-value-bind (key uri lname qname) (klacks:peek source)
    (declare (ignore uri qname))
    (check-type key (member :start-element :end-element))
    lname))

(defun klacks:current-qname (source)
  (multiple-value-bind (key uri lname qname) (klacks:peek source)
    (declare (ignore uri lname))
    (check-type key (member :start-element :end-element))
    qname))

(defun klacks:current-characters (source)
  (multiple-value-bind (key characters) (klacks:peek source)
    (check-type key (member :characters))
    characters))

(defun klacks:consume-characters (source)
  (with-output-to-string (s)
    (while (eq (klacks:peek source) :characters)
      (write-string (klacks:current-characters source) s)
      (klacks:consume source))))

(defun klacks:serialize-event (source handler &key (consume t))
  (multiple-value-bind (key a b c) (klacks:peek source)
    (let ((result nil))
      (case key
	(:start-document
	  (sax:start-document handler)
	  (loop for (prefix . uri) in *initial-namespace-bindings* do
	       (sax:start-prefix-mapping handler prefix uri)))
	(:characters
	  (cond
	    ((klacks:current-cdata-section-p source)
	      (sax:start-cdata handler)
	      (sax:characters handler a)
	      (sax:end-cdata handler))
	    (t
	      (sax:characters handler a))))
	(:processing-instruction
	  (sax:processing-instruction handler a b))
	(:comment
	  (sax:comment handler a))
	(:dtd
	  (sax:start-dtd handler a b (and c (uri-rod c)))
	  (when (slot-boundp source 'internal-declarations)
	    (sax:start-internal-subset handler)
	    (serialize-declaration-kludge
	     (slot-value source 'internal-declarations)
	     handler)
	    (sax:end-internal-subset handler))
	  (serialize-declaration-kludge
	   (slot-value source 'external-declarations)
	   handler)
	  (sax:end-dtd handler)
	  (sax:entity-resolver handler
			       (slot-value source 'dom-impl-entity-resolver))
	  (sax::dtd handler (slot-value source 'dom-impl-dtd)))
	(:start-element
	  (klacks:map-current-namespace-declarations
	   (lambda (prefix uri)
	     (sax:start-prefix-mapping handler prefix uri))
	   source)
	  (sax:start-element handler a b c (klacks:list-attributes source)))
	(:end-element
	  (sax:end-element handler a b c)
	  (klacks:map-current-namespace-declarations
	   (lambda (prefix uri)
	     (declare (ignore uri))
	     (sax:end-prefix-mapping handler prefix))
	   source))
	(:end-document
	 (loop for (prefix . nil) in *initial-namespace-bindings* do
	      (sax:end-prefix-mapping handler prefix))
	  (setf result (sax:end-document handler)))
	((nil)
	  (error "serialize-event read past end of document"))
	(t
	  (error "unexpected klacks key: ~A" key)))
      (when consume
	(klacks:consume source))
      result)))

(defun serialize-declaration-kludge (list handler)
  (loop
      for (fn . args) in list
      do (apply fn handler args)))

(defun klacks:serialize-source (source handler)
  (loop
    (let ((document (klacks:serialize-event source handler)))
      (when document
	(return document)))))

(defclass klacksax (sax:sax-parser)
    ((source :initarg :source)))

(defmethod sax:line-number ((parser klacksax))
  (klacks:current-line-number (slot-value parser 'source)))

(defmethod sax:column-number ((parser klacksax))
  (klacks:current-column-number (slot-value parser 'source)))

(defmethod sax:system-id ((parser klacksax))
  (klacks:current-system-id (slot-value parser 'source)))

(defmethod sax:xml-base ((parser klacksax))
  (klacks:current-xml-base (slot-value parser 'source)))

(defun klacks:serialize-element (source handler &key (document-events t))
  (unless (eq (klacks:peek source) :start-element)
    (error "not at start of element"))
  (sax:register-sax-parser handler (make-instance 'klacksax :source source))
  (when document-events
    (sax:start-document handler))
  (labels ((recurse ()
	     (klacks:serialize-event source handler)
	     (loop
	       (let ((key (klacks:peek source)))
		 (ecase key
		   (:start-element (recurse))
		   (:end-element (return))
		   ((:characters :comment :processing-instruction)
		     (klacks:serialize-event source handler)))))
	     (klacks:serialize-event source handler)))
    (recurse))
  (when document-events
    (sax:end-document handler)))

(defun klacks:find-element (source &optional lname uri)
  (loop
    (multiple-value-bind (key current-uri current-lname current-qname)
	(klacks:peek source)
      (case key
	((nil)
	  (return nil))
	(:start-element
	  (when (and (eq key :start-element)
		     (or (null lname)
			 (equal lname (klacks:current-lname source)))
		     (or (null uri)
			 (equal uri (klacks:current-uri source))))
	    (return
	      (values key current-uri current-lname current-qname)))))
      (klacks:consume source))))

(defun klacks:find-event (source key)
  (loop
    (multiple-value-bind (this a b c)
	(klacks:peek source)
      (cond
	((null this)
	  (return nil))
	((eq this key)
	  (return (values this a b c))))
      (klacks:consume source))))

(define-condition klacks:klacks-error (xml-parse-error) ())

(defun klacks-error (fmt &rest args)
  (%error 'klacks:klacks-error
	  nil
	  (format nil "Klacks assertion failed: ~?" fmt args)))

(defun klacks:expect (source key &optional u v w)
  (multiple-value-bind (this a b c)
      (klacks:peek source)
    (unless (eq this key) (klacks-error "expected ~A but got ~A" key this))
    (when (and u (not (equal a u)))
      (klacks-error "expected ~A but got ~A" u a))
    (when (and v (not (equal b v)))
      (klacks-error "expected ~A but got ~A" v b))
    (when (and w (not (equal c w)))
      (klacks-error "expected ~A but got ~A" w c))
    (values this a b c)))

(defun klacks:skip (source key &optional a b c)
  (klacks:expect source key a b c)
  (klacks:consume source))

(defun invoke-expecting-element (fn source &optional lname uri)
  (multiple-value-bind (key a b)
      (klacks:peek source)
    (unless (eq key :start-element)
      (klacks-error "expected ~A but got ~A" (or lname "element") key))
    (when (and uri (not (equal a uri)))
      (klacks-error "expected ~A but got ~A" uri a))
    (when (and lname (not (equal b lname)))
      (klacks-error "expected ~A but got ~A" lname b))
    (multiple-value-prog1
	(funcall fn)
      (klacks:skip source :end-element a b))))

(defmacro klacks:expecting-element ((source &optional lname uri) &body body)
  `(invoke-expecting-element (lambda () ,@body) ,source ,lname ,uri))
