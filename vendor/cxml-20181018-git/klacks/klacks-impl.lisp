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

(defclass cxml-source (klacks:source)
    (;; args to make-source
     (context :initarg :context)
     (validate :initarg :validate)
     (root :initarg :root)
     (dtd :initarg :dtd)
     (error-culprit :initarg :error-culprit)
     ;; current state
     (continuation)
     (current-key :initform nil)
     (current-values)
     (current-attributes)
     (cdata-section-p :reader klacks:current-cdata-section-p)
     ;; extra WITH-SOURCE magic
     (data-behaviour :initform :DTD)
     (namespace-stack :initform (list *initial-namespace-bindings*))
     (current-namespace-declarations)
     (temporary-streams :initform nil)
     (scratch-pad :initarg :scratch-pad)
     (scratch-pad-2 :initarg :scratch-pad-2)
     (scratch-pad-3 :initarg :scratch-pad-3)
     (scratch-pad-4 :initarg :scratch-pad-4)))

(defmethod klacks:close-source ((source cxml-source))
  (dolist (xstream (slot-value source 'temporary-streams))
    ;; fixme: error handling?
    (close-xstream xstream)))

(defmacro with-source ((source &rest slots) &body body)
  (let ((s (gensym)))
    `(let* ((,s ,source)
	    (*ctx* (slot-value ,s 'context))
	    (*validate* (slot-value ,s 'validate))
	    (*data-behaviour* (slot-value ,s 'data-behaviour))
	    (*namespace-bindings* (car (slot-value ,s 'namespace-stack)))
	    (*scratch-pad* (slot-value ,s 'scratch-pad))
	    (*scratch-pad-2* (slot-value ,s 'scratch-pad-2))
	    (*scratch-pad-3* (slot-value ,s 'scratch-pad-3))
	    (*scratch-pad-4* (slot-value ,s 'scratch-pad-4)))
       (handler-case
	   (with-slots (,@slots) ,s
	     ,@body)
	 (runes-encoding:encoding-error (c)
	   (wf-error (slot-value ,s 'error-culprit) "~A" c))))))

(defun fill-source (source)
  (with-slots (current-key current-values continuation) source
    (unless current-key
      (setf current-key :bogus)
      (setf continuation (funcall continuation))
      (assert (not (eq current-key :bogus))))))

(defmethod klacks:peek ((source cxml-source))
  (with-source (source current-key current-values)
    (fill-source source)
    (apply #'values current-key current-values)))

(defmethod klacks:peek-value ((source cxml-source))
  (with-source (source current-key current-values)
    (fill-source source)
    (apply #'values current-values)))

(defmethod klacks:peek-next ((source cxml-source))
  (with-source (source current-key current-values)
    (setf current-key nil)
    (fill-source source)
    (apply #'values current-key current-values)))

(defmethod klacks:consume ((source cxml-source))
  (with-source (source current-key current-values)
    (fill-source source)
    (multiple-value-prog1
	(apply #'values current-key current-values)
      (setf current-key nil))))

(defmethod klacks:map-attributes (fn (source cxml-source))
  (dolist (a (slot-value source 'current-attributes))
    (funcall fn
	     (sax:attribute-namespace-uri a)
	     (sax:attribute-local-name a)
	     (sax:attribute-qname a)
	     (sax:attribute-value a)
	     (sax:attribute-specified-p a))))

(defmethod klacks:get-attribute
    ((source cxml-source) lname &optional uri)
  (dolist (a (slot-value source 'current-attributes))
    (when (and (equal (sax:attribute-local-name a) lname)
	       (equal (sax:attribute-namespace-uri a) uri))
      (return (sax:attribute-value a)))))

(defmethod klacks:list-attributes ((source cxml-source))
  (slot-value source 'current-attributes))

(defun make-source
    (input &rest args
     &key validate dtd root entity-resolver disallow-internal-subset
	  (buffering t) pathname)
  (declare (ignore validate dtd root entity-resolver disallow-internal-subset
                   pathname))
  (etypecase input
    (xstream
      (when (and (not buffering) (< 1 (runes::xstream-speed input)))
	(warn "make-source called with !buffering, but xstream is buffering"))
      (let ((*ctx* nil))
	(let ((zstream (make-zstream :input-stack (list input))))
	  (peek-rune input)
	  (with-scratch-pads ()
	    (apply #'%make-source
		   zstream
		   (loop
		       for (name value) on args by #'cddr
		       unless (member name '(:pathname :buffering))
		       append (list name value)))))))
    (stream
      (let ((xstream (make-xstream input :speed (if buffering 8192 1))))
	(setf (xstream-name xstream)
	      (make-stream-name
	       :entity-name "main document"
	       :entity-kind :main
	       :uri (safe-stream-sysid input)))
	(apply #'make-source xstream args)))
    (pathname
      (let* ((xstream
	      (make-xstream (open input :element-type '(unsigned-byte 8))
			    :speed (if buffering 8192 1))))
	(setf (xstream-name xstream)
	      (make-stream-name
	       :entity-name "main document"
	       :entity-kind :main
	       :uri (pathname-to-uri (merge-pathnames input))))
	(let ((source (apply #'make-source
			     xstream
			     :pathname input
			     args)))
	  (push xstream (slot-value source 'temporary-streams))
	  source)))
    (rod
      (let ((xstream (string->xstream input)))
	(setf (xstream-name xstream)
	      (make-stream-name
	       :entity-name "main document"
	       :entity-kind :main
	       :uri nil))
	(apply #'make-source xstream args)))
    (array
     (make-source (cxml::make-octet-input-stream input)))))

(defun %make-source
    (input &key validate dtd root entity-resolver disallow-internal-subset
		error-culprit)
  ;; check types of user-supplied arguments for better error messages:
  (check-type validate boolean)
  (check-type dtd (or null extid))
  (check-type root (or null rod))
  (check-type entity-resolver (or null function symbol))
  (check-type disallow-internal-subset boolean)
  (let* ((xstream (car (zstream-input-stack input)))
	 (name (xstream-name xstream))
	 (base (when name (stream-name-uri name)))
	 (context
	  (make-context :main-zstream input
			:entity-resolver entity-resolver
			:base-stack (list (or base ""))
			:disallow-internal-subset disallow-internal-subset))
	 (source
	  (make-instance 'cxml-source
	    :context context
	    :validate validate
	    :dtd dtd
	    :root root
	    :error-culprit error-culprit
	    :scratch-pad *scratch-pad*
	    :scratch-pad-2 *scratch-pad-2*
	    :scratch-pad-3 *scratch-pad-3*
	    :scratch-pad-4 *scratch-pad-4*)))
    (setf (handler context) (make-instance 'klacks-dtd-handler :source source))
    (setf (slot-value source 'continuation)
	  (lambda () (klacks/xmldecl source input)))
    source))

(defun klacks/xmldecl (source input)
  (with-source (source current-key current-values)
    (let ((hd (p/xmldecl input)))
      (setf current-key :start-document)
      (setf current-values
	    (when hd
	      (list (xml-header-version hd)
		    (xml-header-encoding hd)
		    (xml-header-standalone-p hd))))
      (lambda ()
	(klacks/misc*-2 source input
			(lambda ()
			  (klacks/doctype source input)))))))

(defun klacks/misc*-2 (source input successor)
  (with-source (source current-key current-values)
    (multiple-value-bind (cat sem) (peek-token input)
      (case cat
	(:COMMENT
	  (setf current-key :comment)
	  (setf current-values (list sem))
	  (consume-token input)
	  (lambda () (klacks/misc*-2 source input successor)))
	(:PI
	  (setf current-key :processing-instruction)
	  (setf current-values (list (car sem) (cdr sem)))
	  (consume-token input)
	  (lambda () (klacks/misc*-2 source input successor)))
	(:S
	  (consume-token input)
	  (klacks/misc*-2 source input successor))
	(t
	  (funcall successor))))))

(defun klacks/doctype (source input)
  (with-source (source current-key current-values validate dtd)
    (let ((cont (lambda () (klacks/finish-doctype source input)))
	  l)
      (prog1
	  (cond
	    ((eq (peek-token input) :<!DOCTYPE)
	      (setf l (cdr (p/doctype-decl input dtd)))
	      (lambda () (klacks/misc*-2 source input cont)))
	    (dtd
	      (setf l (cdr (synthesize-doctype dtd input)))
	      cont)
	    ((and validate (not dtd))
	      (validity-error "invalid document: no doctype"))
	    (t
	      (return-from klacks/doctype
		(funcall cont))))
	(destructuring-bind (&optional name extid) l
	  (setf current-key :dtd)
	  (setf current-values
		(list name
		      (and extid (extid-public extid))
		      (and extid (extid-system extid)))))))))

(defun klacks/finish-doctype (source input)
  (with-source (source current-key current-values root data-behaviour)
    (ensure-dtd)
    (when root
      (setf (model-stack *ctx*) (list (make-root-model root))))
    (setf data-behaviour :DOC)
    (setf *data-behaviour* :DOC)
    (fix-seen-< input)
    (let* ((final
	    (lambda ()
	      (klacks/eof source input)))
	   (next
	    (lambda ()
	      (setf data-behaviour :DTD)
	      (setf *data-behaviour* :DTD)
	      (klacks/misc*-2 source input final))))
      (klacks/element source input next))))

(defun klacks/eof (source input)
  (with-source (source current-key current-values)
    (p/eof input)
    (klacks:close-source source)
    (setf current-key :end-document)
    (setf current-values nil)
    (lambda () (klacks/nil source))))

(defun klacks/nil (source)
  (with-source (source current-key current-values)
    (setf current-key nil)
    (setf current-values nil)
    (labels ((klacks/done ()
	       (setf current-key nil)
	       (setf current-values nil)
	       #'klacks/done))
      #'klacks/done)))

(defun klacks/element (source input cont)
  (with-source (source current-key current-values current-attributes
		       current-namespace-declarations)
    (with-restored-base-stack (*ctx*)
      (multiple-value-bind (cat n-b new-b uri lname qname attrs) (p/sztag input)
        (setf current-key :start-element)
        (setf current-values (list uri lname qname))
        (setf current-attributes attrs)
        (setf current-namespace-declarations new-b)
        (if (eq cat :stag)
	    (lambda ()
	      (klacks/element-2 source input n-b cont))
	    (lambda ()
	      (klacks/ztag source cont)))))))

(defun klacks/ztag (source cont)
  (with-source (source current-key current-values current-attributes)
    (setf current-key :end-element)
    (setf current-attributes nil)
    (validate-end-element *ctx* (third current-values))
    cont))

(defun klacks/element-2 (source input n-b cont)
  (with-source (source
		current-key current-values current-attributes namespace-stack
		current-namespace-declarations)
    (let ((values* current-values)
          (new-b current-namespace-declarations)
          (ns-stack namespace-stack))
      (setf current-attributes nil)
      (push n-b namespace-stack)
      (let ((finish
	     (lambda ()
	       (setf current-namespace-declarations new-b)
	       (setf namespace-stack ns-stack)
	       (klacks/element-3 source input values* cont))))
	(klacks/content source input finish)))))

(defun klacks/element-3 (source input tag-values cont)
  (with-source (source current-key current-values current-attributes)
    (setf current-key :end-element)
    (setf current-values tag-values)
    (let ((qname (third tag-values)))
      (p/etag input qname)
      (validate-end-element *ctx* qname))
    cont))

(defun klacks/content (source input cont)
  (with-source (source current-key current-values cdata-section-p)
    (let ((recurse (lambda () (klacks/content source input cont))))
      (multiple-value-bind (cat sem) (peek-token input)
	(case cat
	  ((:stag :ztag)
	    (klacks/element source input recurse))
	  ((:CDATA)
	    (process-characters input sem)
	    (setf current-key :characters)
	    (setf current-values (list sem))
	    (setf cdata-section-p nil)
	    recurse)
	  ((:ENTITY-REF)
	    (let ((name sem))
	      (consume-token input)
	      (klacks/entity-reference source input name recurse)))
	  ((:<!\[)
	    (setf current-key :characters)
	    (setf current-values (list (process-cdata-section input)))
	    (setf cdata-section-p t)
	    recurse)
	  ((:PI)
	    (setf current-key :processing-instruction)
	    (setf current-values (list (car sem) (cdr sem)))
	    (consume-token input)
	    recurse)
	  ((:COMMENT)
	    (setf current-key :comment)
	    (setf current-values (list sem))
	    (consume-token input)
	    recurse)
	  (otherwise
	    (funcall cont)))))))

(defun klacks/entity-reference (source zstream name cont)
  (assert (not (zstream-token-category zstream)))
  (with-source (source temporary-streams context)
    (let ((new-xstream (entity->xstream zstream name :general nil)))
      (push new-xstream temporary-streams)
      (push :stop (zstream-input-stack zstream))
      (zstream-push new-xstream zstream)
      (push (stream-name-uri (xstream-name new-xstream)) (base-stack context))
      (let ((next
	     (lambda ()
	       (klacks/entity-reference-2 source zstream new-xstream cont))))
	(etypecase (checked-get-entdef name :general)
	  (internal-entdef
	    (klacks/content source zstream next))
	  (external-entdef
	    (klacks/ext-parsed-ent source zstream next)))))))

(defun klacks/entity-reference-2 (source zstream new-xstream cont)
  (with-source (source temporary-streams context)
    (unless (eq (peek-token zstream) :eof)
      (wf-error zstream "Trailing garbage. - ~S" (peek-token zstream)))
    (assert (eq (peek-token zstream) :eof))
    (assert (eq (pop (zstream-input-stack zstream)) new-xstream))
    (assert (eq (pop (zstream-input-stack zstream)) :stop))
    (pop (base-stack context))
    (setf (zstream-token-category zstream) nil)
    (setf temporary-streams (remove new-xstream temporary-streams))
    (close-xstream new-xstream)
    (funcall cont)))

(defun klacks/ext-parsed-ent (source input cont)
  (with-source (source)
    (when (eq (peek-token input) :xml-decl)
      (let ((hd (parse-text-decl (cdr (nth-value 1 (peek-token input))))))
	(setup-encoding input hd))
      (consume-token input))
    (set-full-speed input)
    (klacks/content source input cont)))


;;;; terrible kludges

(defclass klacks-dtd-handler (sax:default-handler)
    ((handler-source :initarg :source :reader handler-source)
     (internal-subset-p :initform nil :accessor handler-internal-subset-p)))

(defmethod sax:start-internal-subset ((handler klacks-dtd-handler))
  (setf (slot-value (handler-source handler) 'internal-declarations) '())
  (setf (handler-internal-subset-p handler) t))

(defmethod sax:end-internal-subset ((handler klacks-dtd-handler))
  (setf (handler-internal-subset-p handler) nil))

(defmethod sax:entity-resolver ((handler klacks-dtd-handler) fn)
  (setf (slot-value (handler-source handler) 'dom-impl-entity-resolver) fn))

(defmethod sax::dtd ((handler klacks-dtd-handler) dtd)
  (setf (slot-value (handler-source handler) 'dom-impl-dtd) dtd))

(defmethod sax:end-dtd ((handler klacks-dtd-handler))
  (let ((source (handler-source handler)))
    (when (slot-boundp source 'internal-declarations)
      (setf (slot-value source 'internal-declarations)
	    (reverse (slot-value source 'internal-declarations)))
      (setf (slot-value source 'external-declarations)
	    (reverse (slot-value source 'external-declarations))))))

(macrolet
    ((defhandler (name &rest args)
	 `(defmethod ,name ((handler klacks-dtd-handler) ,@args)
	    (let ((source (handler-source handler))
		  (spec (list ',name ,@args)))
	      (if (handler-internal-subset-p handler)
		  (push spec (slot-value source 'internal-declarations))
		  (push spec (slot-value source 'external-declarations)))))))
  (defhandler sax:unparsed-entity-declaration
      name public-id system-id notation-name)
  (defhandler sax:external-entity-declaration
      kind name public-id system-id)
  (defhandler sax:internal-entity-declaration
      kind name value)
  (defhandler sax:notation-declaration
      name public-id system-id)
  (defhandler sax:element-declaration
      name model)
  (defhandler sax:attribute-declaration
      element-name attribute-name type default))


;;;; locator

(defun source-xstream (source)
  (car (zstream-input-stack (main-zstream (slot-value source 'context)))))

(defun source-stream-name (source)
  (let ((xstream (source-xstream source)))
    (if xstream
	(xstream-name xstream)
	nil)))

(defmethod klacks:current-line-number ((source cxml-source))
  (let ((x (source-xstream source)))
    (if x
	(xstream-line-number x)
	nil)))

(defmethod klacks:current-column-number ((source cxml-source))
  (let ((x (source-xstream source)))
    (if x
	(xstream-column-number x)
	nil)))

(defmethod klacks:current-system-id ((source cxml-source))
  (let ((name (source-stream-name source)))
    (if name
	(stream-name-uri name)
	nil)))

(defmethod klacks:current-xml-base ((source cxml-source))
  (let ((x (car (base-stack (slot-value source 'context)))))
    (if (stringp x)
	x
	(puri:render-uri x nil))))

(defmethod klacks:map-current-namespace-declarations (fn (source cxml-source))
  (loop
     for (prefix . uri) in (slot-value source 'current-namespace-declarations)
     do
       (funcall fn prefix uri)))

(defmethod klacks:find-namespace-binding (prefix (source cxml-source))
  (with-source (source)
    (find-namespace-binding prefix)))

(defmethod klacks:decode-qname (qname (source cxml-source))
  (with-source (source)
    (multiple-value-bind (prefix local-name) (split-qname qname)
      (values (and prefix (find-namespace-binding prefix))
	      local-name
	      prefix))))


;;;; debugging

#+(or)
(trace CXML::KLACKS/DOCTYPE
       CXML::KLACKS/EXT-PARSED-ENT
       CXML::KLACKS/MISC*-2
       CXML::KLACKS/ENTITY-REFERENCE
       CXML::KLACKS/ENTITY-REFERENCE-2
       CXML::KLACKS/ELEMENT
       CXML::KLACKS/ZTAG
       CXML::KLACKS/XMLDECL
       CXML::KLACKS/FINISH-DOCTYPE
       CXML::KLACKS/ELEMENT-3
       CXML::KLACKS/EOF
       CXML::KLACKS/ELEMENT-2
       CXML::KLACKS/CONTENT )
