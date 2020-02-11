;;;; dom-sax.lisp -- DOM walker
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: David Lichteblau <david@lichteblau.com>
;;;; Copyright (c) 2004 knowledgeTools Int. GmbH

(cl:in-package #:cxml)

(defun dom:map-document
    (handler document
     &key (include-xmlns-attributes sax:*include-xmlns-attributes*)
	  include-doctype
          include-default-values
	  (recode (and #+rune-is-integer (typep document 'utf8-dom::node))))
  (declare (ignorable recode))
  #+rune-is-integer
  (when recode
    (setf handler (make-recoder handler #'utf8-string-to-rod)))
  (sax:start-document handler)
  (when include-doctype
    (let ((doctype (dom:doctype document)))
      (when doctype
	(sax:start-dtd handler
		       (dom:name doctype)
		       (dom:public-id doctype)
		       (dom:system-id doctype))
	(ecase include-doctype
	  (:full-internal-subset
	    (when (slot-boundp doctype 'dom::%internal-subset)
	      (sax:start-internal-subset handler)
	      (dolist (def (dom::%internal-subset doctype))
		(apply (car def) handler (cdr def)))
	      (sax:end-internal-subset handler)))
	  (:canonical-notations
	    ;; need notations for canonical mode 2
	    (let* ((ns (dom:notations doctype))
		   (a (make-array (dom:length ns))))
	      (when (plusp (dom:length ns))
		(sax:start-internal-subset handler)
		;; get them
		(dotimes (k (dom:length ns))
		  (setf (elt a k) (dom:item ns k)))
		;; sort them
		(setf a (sort a #'rod< :key #'dom:name))
		(loop for n across a do
		      (sax:notation-declaration handler
						(dom:name n)
						(dom:public-id n)
						(dom:system-id n)))
		(sax:end-internal-subset handler)))))
	(sax:end-dtd handler))))
  (labels ((walk (node)
             (dom:do-node-list (child (dom:child-nodes node))
               (ecase (dom:node-type child)
                 (:element
                   (let ((attlist
                          (compute-attributes child
                                              include-xmlns-attributes
                                              include-default-values))
			 (uri (dom:namespace-uri child))
                         (lname (dom:local-name child))
                         (qname (dom:tag-name child)))
                     (sax:start-element handler uri lname qname attlist)
                     (walk child)
                     (sax:end-element handler uri lname qname)))
                 (:cdata-section
                   (sax:start-cdata handler)
                   (sax:characters handler (dom:data child))
                   (sax:end-cdata handler))
                 (:text
                   (sax:characters handler (dom:data child)))
                 (:comment
                   (sax:comment handler (dom:data child)))
                 (:processing-instruction
                   (sax:processing-instruction handler
                                               (dom:target child)
                                               (dom:data child)))))))
    (walk document))
  (sax:end-document handler))

(defun compute-attributes (element xmlnsp defaultp)
  (let ((results '()))
    (dom:do-node-list (a (dom:attributes element))
      (when (and (or defaultp (dom:specified a))
                 (or xmlnsp (not (cxml::xmlns-attr-p (rod (dom:name a))))))
        (push
         (sax:make-attribute :qname (dom:name a)
                             :value (dom:value a)
			     :local-name (dom:local-name a)
			     :namespace-uri (dom:namespace-uri a)
                             :specified-p (dom:specified a))
         results)))
    (reverse results)))
