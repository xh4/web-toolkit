;;;; xml-compat.lisp -- XMLS-compatible data structures
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Developed 2004 for headcraft - http://headcraft.de/
;;;; Copyright: David Lichteblau

(cl:defpackage #:cxml-xmls
  (:use #:cl #:runes)
  (:export #:make-node #:node-name #:node-ns #:node-attrs #:node-children
           #:make-xmls-builder #:map-node #:make-xpath-navigator))

(cl:in-package #:cxml-xmls)


;;;; Knoten

(defun make-node (&key name ns attrs children)
  `(,(if ns (cons name ns) name)
       ,attrs
       ,@children))

(defun node-name (node)
  (let ((car (car node)))
    (if (consp car)
        (car car)
        car)))

(defun (setf node-name) (newval node)
  (let ((car (car node)))
    (if (consp car)
        (setf (car car) newval)
        (setf (car node) newval))))

(defun node-ns (node)
  (let ((car (car node)))
    (if (consp car)
        (cdr car)
        nil)))

(defun (setf node-ns) (newval node)
  (let ((car (car node)))
    (if (consp car)
        (setf (cdr car) newval)
        (setf (car node) (cons car newval)))
    newval))

(defun node-attrs (node)
  (cadr node))

(defun (setf node-attrs) (newval node)
  (setf (cadr node) newval))

(defun node-children (node)
  (cddr node))

(defun (setf node-children) (newval node)
  (setf (cddr node) newval))


;;;; SAX-Handler (Parser)

(defclass xmls-builder (sax:default-handler)
    ((element-stack :initform nil :accessor element-stack)
     (root :initform nil :accessor root)
     (include-default-values :initform t
                             :initarg :include-default-values
                             :accessor include-default-values)
     (include-namespace-uri :initform t
			    :initarg :include-namespace-uri
			    :accessor include-namespace-uri)))

(defun make-xmls-builder (&key (include-default-values t)
		               (include-namespace-uri t))
  "Make a XMLS style builder.  When 'include-namespace-uri is true a modified
  XMLS tree is generated that includes the element namespace URI rather than
  the qualified name prefix and also includes the namespace URI for attributes."
  (make-instance 'xmls-builder
		 :include-default-values include-default-values
		 :include-namespace-uri include-namespace-uri))

(defmethod sax:end-document ((handler xmls-builder))
  (root handler))

(defmethod sax:start-element
    ((handler xmls-builder) namespace-uri local-name qname attributes)
  (let* ((include-default-values (include-default-values handler))
	 (include-namespace-uri (include-namespace-uri handler))
	 (attributes
          (loop
              for attr in attributes
	      for attr-namespace-uri = (sax:attribute-namespace-uri attr)
	      for attr-local-name = (sax:attribute-local-name attr)
              when (and (or (sax:attribute-specified-p attr)
			    include-default-values)
			#+(or)
			(or (not include-namespace-uri)
			    (not attr-namespace-uri)
			    attr-local-name))
              collect
                (list (cond (include-namespace-uri
			     (cond (attr-namespace-uri
				    (cons attr-local-name attr-namespace-uri))
				   (t
				    (sax:attribute-qname attr))))
                            (t
                             (sax:attribute-qname attr)))
                      (sax:attribute-value attr))))
	 (namespace (when include-namespace-uri namespace-uri))
         (node (make-node :name local-name
                          :ns namespace
                          :attrs attributes))
         (parent (car (element-stack handler))))
    (if parent
        (push node (node-children parent))
        (setf (root handler) node))
    (push node (element-stack handler))))

(defmethod sax:end-element
    ((handler xmls-builder) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name qname))
  (let ((node (pop (element-stack handler))))
    (setf (node-children node) (reverse (node-children node)))))

(defmethod sax:characters ((handler xmls-builder) data)
  (let* ((parent (car (element-stack handler)))
         (prev (car (node-children parent))))
    ;; Be careful to accept both rods and strings here, so that xmls can be
    ;; used with strings even if cxml is configured to use octet string rods.
    (if (typep prev '(or rod string))
        ;; um entities herum wird SAX:CHARACTERS mehrfach aufgerufen fuer
        ;; den gleichen Textknoten.  Hier muessen wir den bestehenden Knoten
        ;; erweitern, sonst ist das Dokument nicht normalisiert.
        ;; (XXX Oder sollte man besser den Parser entsprechend aendern?)
        (setf (car (node-children parent))
              (concatenate `(vector ,(array-element-type prev))
                           prev
                           data))
        (push data (node-children parent)))))


;;;; SAX-Treiber (fuer Serialisierung)

(defun map-node
    (handler node
     &key (include-xmlns-attributes sax:*include-xmlns-attributes*)
          (include-namespace-uri t))
  (if include-namespace-uri
      (map-node/lnames (cxml:make-namespace-normalizer handler)
		       node
		       include-xmlns-attributes)
      (map-node/qnames handler node include-xmlns-attributes)))

(defun map-node/lnames (handler node include-xmlns-attributes)
  (sax:start-document handler)
  (labels ((walk (node)
	     (unless (node-ns node)
	       (error "serializing with :INCLUDE-NAMESPACE-URI, but node ~
                       was created without namespace URI"))
	     (let* ((attlist
		     (compute-attributes/lnames node include-xmlns-attributes))
		    (uri (node-ns node))
		    (lname (node-name node))
		    (qname lname)	;let the normalizer fix it
		    )
	       (sax:start-element handler uri lname qname attlist)
	       (dolist (child (node-children node))
		 (typecase child
		   (list (walk child))
		   ((or string rod)
		    (sax:characters handler (string-rod child)))))
	       (sax:end-element handler uri lname qname))))
    (walk node))
  (sax:end-document handler))

(defun map-node/qnames (handler node include-xmlns-attributes)
  (sax:start-document handler)
  (labels ((walk (node)
	     (when (node-ns node)
	       (error "serializing without :INCLUDE-NAMESPACE-URI, but node ~
                       was created with a namespace URI"))
             (let* ((attlist
		     (compute-attributes/qnames node include-xmlns-attributes))
		    (qname (string-rod (node-name node)))
                    (lname (nth-value 1 (cxml::split-qname qname))))
               (sax:start-element handler nil lname qname attlist)
               (dolist (child (node-children node))
                 (typecase child
                   (list (walk child))
                   ((or string rod)
		    (sax:characters handler (string-rod child)))))
               (sax:end-element handler nil lname qname))))
    (walk node))
  (sax:end-document handler))

(defun compute-attributes/lnames (node xmlnsp)
  (remove nil
          (mapcar (lambda (a)
                    (destructuring-bind (name value) a
		      (unless (listp name)
			(setf name (cons name nil)))
                      (destructuring-bind (lname &rest uri) name
			(cond
			  ((not (equal uri "http://www.w3.org/2000/xmlns/"))
			   (sax:make-attribute
			    ;; let the normalizer fix the qname
			    :qname (if uri
				       (string-rod (concatenate 'string
							 "dummy:"
							 lname))
				       (string-rod lname))
			    :local-name (string-rod lname)
			    :namespace-uri uri
			    :value (string-rod value)
			    :specified-p t))
			  (xmlnsp
			   (sax:make-attribute
			    :qname (string-rod
				    (if lname
					(concatenate 'string "xmlns:" lname)
					"xmlns"))
			    :local-name (string-rod lname)
			    :namespace-uri uri
			    :value (string-rod value)
			    :specified-p t))))))
                  (node-attrs node))))

(defun compute-attributes/qnames (node xmlnsp)
  (remove nil
          (mapcar (lambda (a)
                    (destructuring-bind (name value) a
		      (when (listp name)
			(error "serializing without :INCLUDE-NAMESPACE-URI, ~
                                but attribute was created with a namespace ~
                                URI"))
                      (if (or xmlnsp
			      (not (cxml::xmlns-attr-p (string-rod name))))
                          (sax:make-attribute :qname (string-rod name)
                                              :value (string-rod value)
                                              :specified-p t)
                          nil)))
                  (node-attrs node))))

;;;; XPath

(defun make-xpath-navigator ()
  (make-instance 'xpath-navigator))

(defclass xpath-navigator ()
  ((parents :initform (make-hash-table))
   (prefixes :initform (make-hash-table))
   (children :initform (make-hash-table))
   (attributes :initform (make-hash-table))
   (namespaces :initform (make-hash-table))))

(defmethod initialize-instance :after ((instance xpath-navigator) &key)
  (with-slots (prefixes) instance
    (setf (gethash "http://www.w3.org/XML/1998/namespace" prefixes)
	  "xml")))
