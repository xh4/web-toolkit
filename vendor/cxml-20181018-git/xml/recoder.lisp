;;;; recoder.lisp -- SAX handler for string conversion
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Developed 2004 for headcraft - http://headcraft.de/
;;;; Copyright: David Lichteblau

(cl:in-package #:cxml)

(defclass recoder ()
    ((recoder :initarg :recoder :accessor recoder)
     (chained-handler :initarg :chained-handler :accessor chained-handler)))

(defun make-recoder (chained-handler recoder-fn)
  (make-instance 'recoder
    :recoder recoder-fn
    :chained-handler chained-handler))

(macrolet ((%string (rod)
             `(let ((rod ,rod))
                (if (typep rod '(or rod string))
                    (funcall (recoder handler) rod)
                    rod)))
           (defwrapper (name (&rest args) &rest forms)
             `(defmethod ,name ((handler recoder) ,@args)
                (,name (chained-handler handler) ,@forms))))
  (defwrapper sax:start-document ())

  (defwrapper sax:start-element
      (namespace-uri local-name qname attributes)
    (%string namespace-uri)
    (%string local-name)
    (%string qname)
    (mapcar (lambda (attr)
              (sax:make-attribute
               :namespace-uri (%string (sax:attribute-namespace-uri attr))
               :local-name (%string (sax:attribute-local-name attr))
               :qname (%string (sax:attribute-qname attr))
               :value (%string (sax:attribute-value attr))
               :specified-p (sax:attribute-specified-p attr)))
            attributes))

  (defwrapper sax:start-prefix-mapping (prefix uri)
    (%string prefix)
    (%string uri))

  (defwrapper sax:characters (data)
    (%string data))

  (defwrapper sax:processing-instruction (target data)
    (%string target)
    (%string data))

  (defwrapper sax:end-prefix-mapping (prefix)
    (%string prefix))

  (defwrapper sax:end-element (namespace-uri local-name qname)
    (%string namespace-uri)
    (%string local-name)
    (%string qname))

  (defwrapper sax:end-document ())

  (defwrapper sax:comment (data)
    (%string data))

  (defwrapper sax:start-cdata ())

  (defwrapper sax:end-cdata ())

  (defwrapper sax:start-dtd (name public-id system-id)
    (%string name)
    (%string public-id)
    (%string system-id))

  (defwrapper sax:start-internal-subset ())
  (defwrapper sax:end-internal-subset ())

  (defwrapper sax:end-dtd ())

  (defwrapper sax:unparsed-entity-declaration
      (name public-id system-id notation-name)
    (%string name)
    (%string public-id)
    (%string system-id)
    (%string notation-name))

  (defwrapper sax:external-entity-declaration
      (kind name public-id system-id)
    (%string kind)
    (%string name)
    (%string public-id)
    (%string system-id))

  (defwrapper sax:internal-entity-declaration
      (kind name value)
    kind
    (%string name)
    (%string value))

  (defwrapper sax:notation-declaration
      (name public-id system-id)
    (%string name)
    (%string public-id)
    (%string system-id))

  (defwrapper sax:element-declaration (name model)
    (%string name)
    model)

  (defwrapper sax:attribute-declaration
      (element-name attribute-name type default)
    (%string element-name)
    (%string attribute-name)
    (%string type)
    (%string default))

  (defwrapper sax:entity-resolver
      (resolver)
    resolver)

  (defwrapper sax::dtd
      (dtd)
    dtd))
