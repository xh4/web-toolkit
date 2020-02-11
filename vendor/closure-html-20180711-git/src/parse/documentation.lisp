;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SGML; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Documentation strings for the Closure HTML
;;;   Created: 2007-10-20
;;;    Author: David Lichteblau
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2007 David Lichteblau

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :closure-html)

(setf (documentation 'parse 'function)
      "@arg[input]{a pathname, stream, string, or octet array}
       @arg[handler]{nil, or a HAX/SAX handler}
       @return{The return value of this function is determined by the
         @var{handler} argument; see below.}
       @short{Parse the HTML document given as an argument, or referred to
       using a pathname.}

       @var{input} can have one of the following types:
       @begin{itemize}
       @item{pathname -- a Common Lisp pathname. Opens the file specified by
         the pathname parses it as an HTML document.}
       @item{stream -- a Common Lisp stream that has already been opened.}
       @item{array -- an @code{(unsigned-byte 8)} array.  The array is parsed
         directly, and interpreted according to the encoding it specifies.}
       @item{string/rod -- a rod (or string on unicode-capable
         implementations). Parses an XML document from the input string that
         has already undergone external-format decoding.}
       @end{itemize}
       
       If @var{handler} is @code{nil}, the parser's internal representation
       of the document is returned.  The result is equivalent to that
       returned using a PT builder as returned by @fun{make-pt-builder}, but
       avoids creating the same representation twice.
       
       Alternatively, @var{handler} can be a HAX handler
       (see @class{hax:abstract-handler}) or a SAX handler (see the
       @a[http://common-lisp.net/project/cxml/sax.html#sax]{SAX protocol in
       cxml}). In this case, the document will be serialized to the specified
       handler, and the result of @fun{hax:end-document} will be returned
       from this function. Note that the parser will currently always process
       the entire document before sending the first HAX event.")

(setf (documentation 'make-lhtml-builder 'function)
      "@return{The @class{lhtml-builder}, a HAX handler.}
       @short{Create a HAX handler which builds LHTML list structures.}

       Example:
       @begin{pre}
 (chtml:parse \"<p>nada</p>\" (chtml:make-lhtml-builder))
       @end{pre}
       @begin{code}
 => (:HTML NIL (:HEAD NIL) (:BODY NIL (:P NIL \"nada\")))
       @end{code}

       @see{parse}
       @see{serialize-lhtml}")

(setf (documentation 'lhtml-builder 'type)
      "@short{A HAX handler which builds LHTML list structures.}

       LHTML represents each HTML element as a list of the form

  @code{(}@em{name}@code{ (}@em{attributes...}@code{) }@em{children...}@code{)}

       and each attribute as a list of the form

  @code{(}@em{name value}@code{)}
       
       Element and attribute names are symbols in the keyword package
       with uppercase names.  Attribute values are rods or strings.

       @see{make-lhtml-builder}
       @see{serialize-lhtml}")

(setf (documentation 'serialize-lhtml 'function)
      "@arg[document]{an LHTML list}
       @arg[handler]{a HAX/SAX handler}
       @arg[name]{root element name, a rod/string}
       @arg[public-id]{nil or the Public ID, a rod/string}
       @arg[system-id]{nil or the System ID/URI, a rod/string}
       @return{The return value of this function is determined by the
         @var{handler} argument; see below.}
       @short{Serialize the LHTML document into HAX events, sent to the 
         specified HAX handler.}

       @var{handler} can be a HAX handler
       (see @class{hax:abstract-handler}) or a SAX handler (see the
       @a[http://common-lisp.net/project/cxml/sax.html#sax]{SAX protocol in
       cxml}).

       The result of calling @fun{hax:end-document} on the handler will be
       returned from this function.

       If @var{system-id} is specified, a doctype will be written
       according to the arguments @var{name}, @var{public-id}, and
       @var{system-id}.

       Use this function with a serialization sink to get a string or file
       with a serialized HTML document, or with a HAX/SAX builder to
       convert LHTML into a different representation, like DOM, PT, or
       STP.

       Example:
       @begin{pre}
 (let ((x '(:HTML NIL (:HEAD NIL) (:BODY NIL (:P NIL \"nada\"))))))
   (chtml:serialize-lhtml x (chtml:make-string-sink))
       @end{pre}
       @begin{code}
 => \"<HTML><HEAD></HEAD><BODY><P>nada</P></BODY></HTML>\"
       @end{code}

       @see{parse}
       @see{make-lhtml-builder}")

(setf (documentation 'make-pt-builder 'function)
      "@return{The @class{pt-builder}, a HAX handler.}
       @short{Create a HAX handler which builds @class{pt} structures.}

       Example:
       @begin{pre}
 (chtml:parse \"<p>nada</p>\" (chtml:make-pt-builder))
       @end{pre}
       @begin{code}
 => #<SGML:PT HTML ..>
       @end{code}

       @see{parse}
       @see{serialize-pt}")

(setf (documentation 'pt-builder 'type)
      "@short{A HAX handler which builds PT structures.}

       PT represents each HTML element as a structure instance of type
       @class{pt}.

       @see{make-pt-builder}
       @see{serialize-pt}")

(setf (documentation 'serialize-pt 'function)
      "@arg[document]{an @class{pt} instance}
       @arg[handler]{a HAX/SAX handler}
       @arg[name]{root element name, a rod/string}
       @arg[public-id]{nil or the Public ID, a rod/string}
       @arg[system-id]{nil or the System ID/URI, a rod/string}
       @return{The return value of this function is determined by the
         @var{handler} argument; see below.}
       @short{Serialize the PT node into HAX events, sent to the 
         specified HAX handler.}

       @var{handler} can be a HAX handler
       (see @class{hax:abstract-handler}) or a SAX handler (see the
       @a[http://common-lisp.net/project/cxml/sax.html#sax]{SAX protocol in
       cxml}).

       The result of calling @fun{hax:end-document} on the handler will be
       returned from this function.

       If @var{system-id} is specified, a doctype will be written
       according to the arguments @var{name}, @var{public-id}, and
       @var{system-id}.

       Use this function with a serialization sink to get a string or file
       with a serialized HTML document, or with a HAX/SAX builder to
       convert PT into a different representation, like DOM, LHTML, or
       STP.

       Example:
       @begin{pre}
 (let ((x (chtml:parse \"<p>nada</p>\" (chtml:make-pt-builder)))))
   (chtml:serialize-pt x (chtml:make-string-sink))
       @end{pre}
       @begin{code}
 => \"<HTML><HEAD></HEAD><BODY><P>nada</P></BODY></HTML>\"
       @end{code}

       @see{parse}
       @see{make-pt-builder}")

(setf (documentation 'pt 'type)
      "@short{Represents an HTML element.}

       PT represents each HTML element as a structure instance, named by
       a keyword symbol.  The children of a PT node are strings (rods)
       for text nodes, or other PT nodes for elements.

       @see{make-pt-builder}
       @see{serialize-pt}
       @see-slot{pt-name}
       @see-slot{pt-children}
       @see-slot{pt-parent}
       @see-slot{pt-attrs}")

(setf (documentation 'pt-name 'function)
      "@arg[instance]{a @class{pt} node}
       @return{a keyword symbol}
       @short{Returns the element's name.}

       HTML element names are symbols in the keyword package with uppercase
       names.")

(setf (documentation 'pt-children 'function)
      "@arg[instance]{a @class{pt} node}
       @return{a list}
       @short{Returns the element's children.}

       The children of a PT node are strings (rods)
       for text nodes, or other PT nodes for elements.

       @see{pt-parent}")

(setf (documentation 'pt-parent 'function)
      "@arg[instance]{a @class{pt} node}
       @return{nil, or a @class{pt} node}
       @short{Returns the element's parent node.}

       This slot should refer to the node's parent, if it is included
       in the list of that node's children.

       @see{pt-children}")

(setf (documentation 'pt-attrs 'function)
      "@arg[instance]{a @class{pt} node}
       @return{a plist}
       @short{Returns the element's attributes as a plist.}

       This plist maps attribute names to their values. 

       Attribute names are symbols in the keyword package with uppercase
       names.  Attribute values are strings/rods.")

(setf (documentation 'make-octet-vector-sink 'function)
      "@return{a HAX handler}
       @short{Returns a sink creating octet vectors.}

       This function creates a serialization sink.  Sinks are HAX handlers
       that write events in their normal HTML syntax, and return
       the result from @fun{hax:end-document}, if applicable.

       This particular kind of sink creates an HTML document in an array
       of @code{(unsigned-byte 8)}.

       @see{make-character-stream-sink}
       @see{make-octet-stream-sink}
       @see{make-rod-sink}
       @see{make-string-sink}")

(setf (documentation 'make-octet-stream-sink 'function)
      "@return{a HAX handler}
       @short{Returns a sink writing to an octet stream.}

       This function creates a serialization sink.  Sinks are HAX handlers
       that write events in their normal HTML syntax.

       This particular kind of sink writen the HTML document to a stream
       of element-type @code{(unsigned-byte 8)}.

       @see{make-character-stream-sink}
       @see{make-octet-vector-sink}
       @see{make-rod-sink}
       @see{make-string-sink}")

#+rune-is-character
(setf (documentation 'make-string-sink 'function)
      "@return{a HAX handler}
       @short{Returns a sink creating strings.}

       This function creates a serialization sink.  Sinks are HAX handlers
       that write events in their normal HTML syntax, and return
       the result from @fun{hax:end-document}, if applicable.

       This particular kind of sink creates an HTML document in a string.
       The string is @em{not} encoded into an external-format.  When
       writing this string to a Lisp character stream at a later point, make
       sure that the stream's external format agrees with the encoding
       declared by the document, if any.

       @b{Supported only on Lisps with Unicode support.}  On Lisps without
       Unicode characters, try @em{make-string-sink/utf8} as an alternative
       that has different encoding behaviour, but still uses strings.  Or
       use @em{make-rod-sink}, which creates arrays of code points.

       @see{make-character-stream-sink}
       @see{make-octet-stream-sink}
       @see{make-octet-vector-sink}
       @see{make-rod-sink}")

(setf (documentation 'make-rod-sink 'function)
      "@return{a HAX handler}
       @short{Returns a sink creating rods.}

       This function creates a serialization sink.  Sinks are HAX handlers
       that write events in their normal HTML syntax, and return
       the result from @fun{hax:end-document}, if applicable.

       This particular kind of sink creates an HTML document in a rod.

       On Lisps with Unicode support, @code{make-string-sink} is an alias for
       this function.

       @see{make-character-stream-sink}
       @see{make-octet-stream-sink}
       @see{make-octet-vector-sink}
       @see{make-string-sink}")

#+rune-is-character
(setf (documentation 'make-character-stream-sink 'function)
      "@return{a HAX handler}
       @short{Returns a sink writing to a character stream.}

       This function creates a serialization sink.  Sinks are HAX handlers
       that write events in their normal HTML syntax.

       This particular kind of sink writen the HTML document to a stream
       of element-type @code{character}. The characters written are @em{not}
       encoded into an external-format. Make sure that the stream's external
       format agrees with the encoding declared by the document, if any.

       @b{Supported only on Lisps with Unicode support.}  On Lisps without
       Unicode characters, try @em{make-character-stream-sink/utf8} as
       an alternative that has different encoding behaviour, but still uses
       character streams.

       @see{make-octet-stream-sink}
       @see{make-octet-vector-sink}
       @see{make-rod-sink}
       @see{make-string-sink}")

(setf (documentation 'with-element 'function)
      "@arg[name]{the element's name, a string/rod}
       @arg[body]{an implicit progn}
       @return{the value of @var{body}}
       @short{Generate @fun{hax:start-element} and @fun{hax:end-element}
         events.}

       Execute @var{body} as an implicit progn.  Send a start-element event to
       the current sink (before the first child element begins, or the
       current element ends), including all attributes specified using
       @fun{attribute} until that point.  Send an end-element event after
       @var{body} is finished.

       To be used in the extent of an @fun{with-html-output} invocation.")

(setf (documentation 'with-output-sink 'function)
      "@arg[var]{the variable name, a symbol}
       @arg[body]{an implicit progn}
       @return{the value of @var{body}}
       @short{Bind a variable to the current serialization sink.}

       Execute @var{body} as an implicit progn with @var{var} bound to a
       serialization sink that @var{body} can safely call HAX functions on.

       To be used in the extent of an @fun{with-html-output} invocation.")

(setf (documentation 'with-html-output 'function)
      "@arg[sink]{a HAX/SAX handler}
       @arg[name]{root element name, a rod/string}
       @arg[public-id]{nil or the Public ID, a rod/string}
       @arg[system-id]{nil or the System ID/URI, a rod/string}
       @arg[body]{an implicit progn}
       @return{the value of @var{body}}
       @short{Generate @fun{hax:start-document} and @fun{hax:end-document}
         events.}

       Send a start-document event to the current sink, then execute
       @var{body} as an implicit progn.  Afterwards, send an end-element
       event.

       @see{with-output-sink}
       @see{with-element}
       @see{attribute}
       @see{text}
       @see{comment}")

(setf (documentation 'attribute 'function)
      "@arg[name]{a string/rod}
       @arg[value]{a string/rod or other object}
       @return{the @var{value}}
       @short{Add an attribute to the current element.}

       To be used in the extent of an @fun{with-element} invocation, this
       function adds an attribute to the element being serialized.")

(setf (documentation 'text 'function)
      "@arg[data]{a string/rod}
       @return{the @var{data}}
       @short{Write a text node.}

       To be used in the extent of an @fun{with-html-output} invocation, this
       function serializes a text node.")

(setf (documentation 'comment 'function)
      "@arg[data]{a string/rod}
       @return{the @var{data}}
       @short{Write a comment node.}

       To be used in the extent of an @fun{with-html-output} invocation, this
       function serializes a comment.")

(setf (documentation '*html-dtd* 'variable)
      "fixme: exported only for the benefit of Closure")
