;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: An event API for the HTML parser, inspired by SAX
;;;   Created: 2007-10-14
;;;    Author: David Lichteblau
;;;   License: BSD
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005,2007 David Lichteblau

;;; Redistribution and use  in source and binary   forms, with or  without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;;
;;; 1. Redistributions  of  source  code  must retain  the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in  binary form must reproduce  the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution
;;;
;;; THIS  SOFTWARE   IS PROVIDED ``AS  IS''   AND ANY  EXPRESS  OR IMPLIED
;;; WARRANTIES, INCLUDING, BUT NOT LIMITED  TO, THE IMPLIED WARRANTIES  OF
;;; MERCHANTABILITY  AND FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;;; INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO,   PROCUREMENT OF SUBSTITUTE GOODS   OR
;;; SERVICES;  LOSS OF  USE,  DATA, OR  PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER  CAUSED AND ON ANY THEORY  OF LIABILITY,  WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;; IN ANY WAY  OUT OF THE  USE OF THIS SOFTWARE,  EVEN IF ADVISED OF  THE
;;; POSSIBILITY OF SUCH DAMAGE.

(defpackage :hax
  (:use :common-lisp)
  (:export #:abstract-handler
	   #:default-handler

	   #:make-attribute
	   #:standard-attribute
           #:find-attribute
           #:attribute-name
           #:attribute-value
           #:attribute-specified-p

           #:start-document
           #:start-element
           #:characters
           #:unescaped
           #:end-element
           #:end-document
           #:comment

	   #+rune-is-integer
	   #:%want-strings-p))

(in-package :hax)


;;;; ATTRIBUTE

(defgeneric attribute-name (attribute))
(defgeneric attribute-value (attribute))
(defgeneric attribute-specified-p (attribute))

(defclass standard-attribute ()
  ((name :initarg :name :accessor attribute-name)
   (value :initarg :value :accessor attribute-value)
   (specified-p :initarg :specified-p :accessor attribute-specified-p)))

(defun make-attribute (name value &optional (specified-p t))
  (make-instance 'standard-attribute
                 :name name
                 :value value
                 :specified-p specified-p))

(defun %rod= (x y)
  ;; allow rods *and* strings *and* null
  (cond
    ((zerop (length x)) (zerop (length y)))
    ((zerop (length y)) nil)
    ((stringp x) (string= x y))
    (t (runes:rod= x y))))

(defun find-attribute (name attrs)
  (find name attrs :key #'attribute-name :test #'%rod=))


;;;; ABSTRACT-HANDLER and DEFAULT-HANDLER

(defclass abstract-handler () ())
(defclass default-handler (abstract-handler) ())

#+rune-is-integer
(defgeneric %want-strings-p (handler)
  (:method ((handler null)) nil)
  (:method ((handler abstract-handler)) t))

(defgeneric start-document (handler name public-id system-id)
  (:method ((handler null) name public-id system-id)
    (declare (ignore name public-id system-id))
    nil)
  (:method ((handler default-handler) name public-id system-id)
    (declare (ignore name public-id system-id))
    nil))

(defgeneric start-element (handler name attributes)
  (:method ((handler null) name attributes)
    (declare (ignore name attributes))
    nil)
  (:method ((handler default-handler) name attributes)
    (declare (ignore name attributes))
    nil))

(defgeneric characters (handler data)
  (:method ((handler null) data)
    (declare (ignore data))
    nil)
  (:method ((handler default-handler) data)
    (declare (ignore data))
    nil))

(defgeneric unescaped (handler data)
  (:method ((handler null) data)
    (declare (ignore data))
    nil)
  (:method ((handler default-handler) data)
    (declare (ignore data))
    nil))

(defgeneric end-element (handler name)
  (:method ((handler null) name)
    (declare (ignore name))
    nil)
  (:method ((handler default-handler) name)
    (declare (ignore name))
    nil))

(defgeneric end-document (handler)
  (:method ((handler null)) nil)
  (:method ((handler default-handler)) nil))

(defgeneric comment (handler data)
  (:method ((handler null) data)
    (declare (ignore data))
    nil)
  (:method ((handler default-handler) data)
    (declare (ignore data))
    nil))


;;;; documentation

(setf (documentation (find-package :hax) t)
      "An event protocol for HTML serialization, this package is similar
       to the SAX protocol defined by cxml for XML serialization.

       (Technically, this package should have been spelled SAH, but HAX
       sounds better.)

       Note that Closure HTML is not a streaming parser yet.  Documents
       are always parsed in full before the first HAX event is emitted.
       In spite of this restriction, the HAX API is useful for HTML
       serialization and transformation purposes, and for integration
       with SAX.

       @begin[HAX handlers]{section}
       @aboutclass{abstract-handler}
       @aboutclass{default-handler}
       @end{section}
       @begin[The attribute protocol]{section}
       @aboutclass{standard-attribute}
       @aboutfun{make-attribute}
       @aboutfun{attribute-name}
       @aboutfun{attribute-value}
       @aboutfun{attribute-specified-p}
       @end{section}
       @begin[HAX events]{section}
       @aboutfun{start-document}
       @aboutfun{start-element}
       @aboutfun{end-element}
       @aboutfun{characters}
       @aboutfun{unescaped}
       @aboutfun{comment}
       @aboutfun{end-document}
       @end{section}")

(setf (documentation 'abstract-handler 'type)
      "@short{The superclass of all HAX handlers.}

       Direct subclasses have to implement all event methods, since
       no default methods are defined on this class.

       Note that it is permissible to use handlers that are not
       instances of this class in some circumstances.

       In particular,
       @code{nil} is a valid HAX handler and ignores all events.

       In addition,
       @a[http://common-lisp.net/project/cxml/sax.html#sax]{SAX handlers}
       are valid HAX handlers (and vice versa), even though
       hax:abstract-handler and sax:abstract-handler do not
       share a specific superclass.  HAX events sent to SAX handlers are
       automatically re-signalled as XHTML SAX events, and SAX events sent
       to HAX handlers are re-signalled as namespace-less HAX events.

       However, user code should define subclasses of the documented
       superclasses to enable the HAX/SAX bridging described above.

       @see{chtml:parse}
       @see{chtml:serialize-lhtml}
       @see{chtml:serialize-pt}
       @see{start-document}
       @see{end-document}
       @see{start-element}
       @see{end-element}
       @see{characters}
       @see{unescaped}
       @see{comment}")

(setf (documentation 'default-handler 'type)
      "@short{A no-op HAX handler.}

       This class defines methods for all HAX events that do nothing.
       It is useful as a superclass when implementing a HAX handler that
       is interested in only some events and not others.

       @see{chtml:parse}
       @see{chtml:serialize-lhtml}
       @see{chtml:serialize-pt}
       @see{start-document}
       @see{end-document}
       @see{start-element}
       @see{end-element}
       @see{characters}
       @see{unescaped}
       @see{comment}")

(setf (documentation 'standard-attribute 'type)
      "@short{An implementation of the HAX attribute protocol.}

       A standard class implementing the generic functions for HAX
       attributes.  Instances of this class can be passed to
       @fun{hax:start-element} in the list of attributes.

       @see-slot{attribute-name}
       @see-slot{attribute-value}
       @see-slot{attribute-specified-p}
       @see-constructor{make-instance}")

(setf (documentation 'make-attribute 'function)
      "@arg[name]{a string/rod}
       @arg[value]{a string/rod}
       @arg[specified-p]{a boolean, default is @code{t}}
       @return{an instance of @class{standard-attribute}.}
       @short{Creates a HAX attribute.}

       Creates an instance that can be used with the generic functions
       for HAX attributes.  The result can be passed to
       @fun{hax:start-element} in the list of attributes.

       @see{attribute-name}
       @see{attribute-value}
       @see{attribute-specified-p}")

(setf (documentation 'find-attribute 'function)
      "@arg[name]{a string/rod}
       @arg[attrs]{a list of attributes}
       @return{an attribute, or nil}
       @short{Searches for an attribute by name.}

       Returns the first attribute in @var{attrs} with the specified name,
       or @code{nil} if no such attribute was found.

       @see{attribute-name}")

(setf (documentation 'attribute-name 'function)
      "@arg[instance]{any class implementing this function}
       @return{a string/rod}
       @short{Return an attribute's name.}

       Instances of this classes implementing this function can be passed to
       @fun{hax:start-element} in the list of attributes.

       @see{attribute-value}
       @see{attribute-specified-p}")

(setf (documentation 'attribute-value 'function)
      "@arg[instance]{any class implementing this function}
       @return{a string/rod}
       @short{Return an attribute's value.}

       Instances of this classes implementing this function can be passed to
       @fun{hax:start-element} in the list of attributes.

       @see{attribute-name}
       @see{attribute-specified-p}")

(setf (documentation 'attribute-specified-p 'function)
      "@arg[instance]{any class implementing this function}
       @return{a string/rod}
       @short{Return whether the attribute was contained the parsed document.}

       Attributes return @code{nil} here if they resulted from a default
       value declaration in a DTD.

       Instances of this classes implementing this function can be passed to
       @fun{hax:start-element} in the list of attributes.

       @see{attribute-name}
       @see{attribute-value}")

(setf (documentation 'start-document 'function)
      "@arg[handler]{a HAX/SAX handler
         (see @class{abstract-handler} for details)}
       @arg[name]{root element name, a rod/string}
       @arg[public-id]{nil or the Public ID, a rod/string}
       @arg[system-id]{nil or the System ID/URI, a rod/string}
       @return{unspecified}
       @short{Signals the beginning of an HTML document.}

       This is the first event sent to any handler.

       If @var{system-id} is non-nil, the document includes a doctype
       declaration.

       @see{start-element}
       @see{end-element}
       @see{characters}
       @see{unescaped}
       @see{comment}
       @see{end-document}")

(setf (documentation 'start-element 'function)
      "@arg[handler]{a HAX/SAX handler
         (see @class{abstract-handler} for details)}
       @arg[name]{root element name, a rod/string}
       @arg[attributes]{a list of attributes}
       @return{unspecified}
       @short{Signals the beginning of an HTML element.}

       This event corresponds to the opening tag of an element.

       Elements of the attribute list can have any class, but must implement
       the generic functions for attributes.  See @class{standard-attribute}
       for the built-in attribute implementation.

       @see{find-attribute}
       @see{start-document}
       @see{end-element}
       @see{characters}
       @see{unescaped}
       @see{comment}
       @see{end-document}")

(setf (documentation 'end-element 'function)
      "@arg[handler]{a HAX/SAX handler
         (see @class{abstract-handler} for details)}
       @arg[name]{root element name, a rod/string}
       @return{unspecified}
       @short{Signals the end of an HTML element.}

       This event corresponds to the closing tag of an element.

       @see{start-document}
       @see{start-element}
       @see{characters}
       @see{unescaped}
       @see{comment}
       @see{end-document}")

(setf (documentation 'characters 'function)
      "@arg[handler]{a HAX/SAX handler
         (see @class{abstract-handler} for details)}
       @arg[data]{rod/string}
       @return{unspecified}
       @short{Signals character data.}

       This event represents character data in a document.

       @see{start-document}
       @see{start-element}
       @see{end-element}
       @see{comment}
       @see{end-document}")

(setf (documentation 'unescaped 'function)
      "@arg[handler]{a HAX/SAX handler
         (see @class{abstract-handler} for details)}
       @arg[data]{rod/string}
       @return{unspecified}
       @short{Escaping bypass.}

       This event writes raw characters into a document.

       Beware dragons.

       @see{start-document}
       @see{start-element}
       @see{end-element}
       @see{comment}
       @see{end-document}")

(setf (documentation 'comment 'function)
      "@arg[handler]{a HAX/SAX handler
         (see @class{abstract-handler} for details)}
       @arg[data]{rod/string}
       @return{unspecified}
       @short{Signals a comment.}

       This event represents a comment.

       @see{start-document}
       @see{start-element}
       @see{end-element}
       @see{characters}
       @see{unescaped}
       @see{end-document}")

(setf (documentation 'end-document 'function)
      "@arg[handler]{a HAX/SAX handler
         (see @class{abstract-handler} for details)}
       @return{The return value of this function depends on the handler class.}
       @short{Signals the end of an HTML document.}

       This is the last event sent to any handler, and signals the end of
       serialization.

       The return value of this function is usually returned to user code
       by higher-level serialization functions and can be considered the
       result of serialization and \"return value\" of the handler.

       @see{start-document}
       @see{start-element}
       @see{end-element}
       @see{characters}
       @see{unescaped}
       @see{comment}")
