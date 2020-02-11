;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SGML; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: An SGML Parser
;;;   Created: 1996-10-21
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1996-1999 by Gilbert Baumann

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

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  2001-05-17  GB      - empty tags are threated as start tags.
;;                        [we can do better than that].
;;
;;  2001-05-14  GB      - finally all mumbling goes with PARSE-WARN.
;;
;;  1999-09-19  GB      - a-streams are gone now, we use the xstreams from the 
;;                        XML parser.
;;
;;  1999-08-24  GB      = new scheme to handle unsyntactic nesting of FONT tags
;;                        thru' post mortem analysis; due to that:
;;                        - *FONT-HEURISTIC-P*: new special variable
;;                        - HTAG-NODE: new structure type
;;                        - New stack machine output token type :HTAG
;;                        - TAG-AS-MARKER-P: new predicate
;;                        - POST-MORTEM-HEURISTIC: new function
;;                        - PARSE-HTML: use it
;;                        - POST-MORTEM/FIX-TOP-LEVEL-STRUCTURE: moved code
;;                          from PARSE-HTML here
;;
;;                      - HTML-PARSE-FILE, HTML-PARSE-URL: new functions
;;
;;  1999-08-23  GB      - EMPTY-ELEMENT-P: new function
;;                      - SGML-PARSE, TRANSITION: new output token :open/close
;;                        to accommodate empty elements (like <IMG>, <BR>)
;;
;;  1999-08-19  GB      - nuked FAT-WHITE-SPACE-P
;;                      - nuked U16->STRING (using ROD-STRING now).
;;                      - nuked VECTOR/U16
;;                      - use #/.. read syntax instead of #.(char-code) idiom
;;                      - use RUNE=, RUNE<= et al
;;                      - use ROD= et al
;;			- twixed SUBSEQ/U16 to SUBSEQ/ROD
;;			- prays that everything still works.

(in-package :SGML)

#+:CMU
  (shadow "CODE-CHAR")
#+:CMU
  (declaim (inline code-char))
#+:CMU
  (defun code-char (code)
    (if (< code char-code-limit)
        (cl:code-char code)
        nil))

;; This is a high-speed implementation of an SGML parser.

;; NOTES

;;  o We do our own buffering here, which allows us to access the buffer 
;;    directly and thus cutting down the number of function calls per character
;;    drastically.

;;  o The DTD has to be 'compiled' before we drive a little determistic PDA
;;    from it.

;;; TODO 

;;  o das mit dem <body> = full speed functioniert irgentwie nicht
;;    richtig ;-( wenn das BODY tag fehlt. -- es kommt nur etwas spaet
;;    wegen token lookahead, was aber kein problem sein sollte.

;;  o improve error handling
;;    - After we have encountered an error while reading a tag nuke everything
;;      until '>'.
;;    - Maybe? 
;;      Forgive something like <foo x="10%> and back up until #/" by patching
;;      the read buffer
;;    - emit warning if somebody writes e.g. <A href=http://foo/bar/baz>,
;;      which is illegal

;;  o in SGML parser:
;;    - account for the SGML inclusion '+(..)' and exclusion '-(..)'  feature
;;    - implement the HTML idea of CDATA for SCRIPT and (uhm!) XMP.
;;    - thanks to the W3C: '<' and '>' are now part of CSS-2 syntax so we
;;      need something similar for STYLE also.

;;  o merge as much is possible with the XML parser in a modular fashion

;;  o scratch-pad hack needs to be reentrant !!!
;;    [make xml-parser's implementation of with-rune-collector safe 
;;    and use that?]

;;  o the FORM hack should be there.
;;    (we also need an <INPUT> hack).

;;  o finally ensure that the rune abstraction is fine.

;;  o report errors using exactly one mechanism and include file name
;;    (uri) into error messages; better yet: define a error message
;;    structure -- in the future it should be possible to have something 
;;    like an error message listener.

;;  o since we use xstreams now morph the eof representation from NIL
;;    to :EOF. Or better yet: give the READ-RUNE macro a standard
;;    signature including eof-error-p and eof-value. (Wouldn't hurt,
;;    since it is a macro).

;;  o In DTD abstraction: ELM-INCLUSION needs flag error-p

;;  o we need rune-max and therefore are not dependent on UCS-2
;;    anymore, should the need araise simply redefine rune-max.

;;; IDEA

;;  Build the parse tree out of lazy lists. Incremental rendering would
;;  then become cheap to implement.

(defparameter *preserves-comments-elements* 
    '(:STYLE :SCRIPT)
  "List of names of elements, which should preserve (may contain) comment tokens.")

(defparameter *font-heuristic-p* t
  "Whether to handle FONT by heuristic and post mortem processing.")

(defparameter *anchor-heuristic-p* nil
  "Whether to handle A tags by heuristic and post mortem processing.")

(defparameter *gt-ends-comment-p* nil
  "Whether and '>' ends a comment? -- A warning is emitted still.")

(defvar *options/parser-silent-p* nil)
(defvar *line-number*)
(defparameter *parse-warn-level* 5)


;; HTAG = heuristic tag
;; These are unparsed tags, which are inserted verbatim into the parse tree
;; for post mortem analysis.
;; See explaination in FONT element fixer far below.

(defstruct (htag-node (:include pt)) )          ;heuristic tag
(defstruct (hstag-node (:include htag-node)) )  ;heuristic start tag
(defstruct (hetag-node (:include htag-node)) )  ;heuristic end tag

;; The predicate TAG-AS-MARKER-P decides, whether a given tag is
;; inserted as HSTAG or HETAG.

;;;; --------------------------------------------------------------------------
;;;;  Predicates and Utilities on Runes

;; Most of these should really go into util.lisp

(definline name-start-rune-p (char)
  (or (rune<= #/a char #/z)
      (rune<= #/A char #/Z)))

(definline name-rune-p (char)
  (or (name-start-rune-p char)
      (digit-rune-p char) 
      (rune= char #/.) 
      (rune= char #/-)
      (rune= char #/:)))

(definline sloopy-name-rune-p (char)
  (or (name-rune-p char)
      (rune= char #/%)
      (rune= char #//)      ;manche schreiben ganze urls ohne Gaensefuesschen
      (rune= char #/:)
      (rune= char #/~)
      (rune= char #/#)      ;farben werden auch gerne genommen
      (rune= char #/_)
      (rune= char #/+)
      (rune= char #/?)
      (rune= char #/!)
      (rune= char #/@)
      ;; ganzer ECMA-Script Code kommt auch vor
      (rune= char #/\()
      (rune= char #/\))
      (rune= char #/')
      (rune= char #/\")
      (rune= char #/\;)
      (rune= char #/,)
      (rune= char #/[)
      (rune= char #/])
      (rune= char #/&) ))

(definline sloopy-value-rune-p (char)
  (or (sloopy-name-rune-p char)
      (rune= char #/=)))

(definline alpha-rune-p (char)
  (or (rune<= #/a char #/z)
      (rune<= #/A char #/Z)))

(definline upcase-name-rune (rune)
  (rune-upcase rune))

;;;; --------------------------------------------------------------------------
;;;;  Rod Utilities

(definline subseq/rod (source start end)
  ;; Optimized version of subseq for arrays of runes
  (declare (type rod source)
           (type fixnum start end))
  (let ((res (make-rod (- end start))))
    (declare (type rod res))
    (do ((i (- (- end start) 1) (the fixnum (- i 1))))
        ((< i 0) res)
      (declare (type fixnum i))
      (setf (%rune res i) (%rune source (the fixnum (+ i start)))))))

;;; ---------------------------------------------------------------------------
;;;  Buffered I/O

(eval-when (compile load eval)
  (defparameter *buf-size* 4096))

(defmacro a-read-byte (input)
  (let ((c (gensym)))
    `(let ((,c (runes:read-rune ,input)))
       (if (eq ,c :eof) nil ,c))))

(defmacro a-peek-byte (input)
  (let ((c (gensym)))
    `(let ((,c (runes:peek-rune ,input)))
       (if (eq ,c :eof) nil ,c))))

(defmacro a-unread-byte (byte input)
  `(runes:unread-rune ,byte ,input))

(defmacro a-stream-position (input)
  `(runes:xstream-position ,input))

(defun make-a-stream (&key cl-stream)
  (runes:make-xstream cl-stream :initial-speed 1 :speed 8192))

(defmethod runes::read-octets (sequence (stream html-glisp:gstream) start end)
  (html-glisp:g/read-byte-sequence sequence stream :start start :end end))

(defmethod runes::xstream/close ((stream html-glisp:gstream))
  (html-glisp:g/close stream))

;; a fake definition -- XXX non-reentrant!

(defun a-stream-scratch (input)
  (getf (runes::xstream-plist input) 'scratch-pad))

(defun (setf a-stream-scratch) (new-value input)
  (setf (getf (runes::xstream-plist input) 'scratch-pad) new-value))

;;;; -------------------------------------------------------------------------
;;;;  Reporting Errors
;;;;

(defun read-tag-error (input format-string &rest format-args)
  (apply #'parse-warn input 4 format-string format-args)
  (throw 'read-tag-error 
    (values :pcdata
	    (string-rod "##BAD TAG##"))))

;;; Warning Levels
;;; --------------
;;;  1 - Absolutely normal mumbleing
;;;  2 - Laziness introduced by the HTML standard
;;;  3 - Semantic error upon parsing attributes
;;;  4 - Accidents happening while parsing the structure
;;;  5 - More serve errors
;;;

(defun parse-warn (input level fmt &rest args)
  (let ((*print-pretty* nil))		;disable ugly^H^H^H^Hpretty printing 
    (when (>= level *parse-warn-level*)
      (unless *options/parser-silent-p*
        (let ((preample (format nil ";; Parser warning: ~11@<Line ~D,~> ~11@<column ~D~>: ~5A "
                                (and input (ignore-errors (runes:xstream-line-number input)))
                                (and input (ignore-errors (runes:xstream-column-number input)))
                                (make-string level :initial-element #\*))))
          (fresh-line *trace-output*)
          (write-string preample *trace-output*)
          (with-input-from-string (i (apply #'format nil fmt args))
            (do ((x (read-line i nil nil) (read-line i nil nil))
                 (firstp t nil))
                ((null x))
              (unless firstp
                (write-string ";; " *trace-output*)
                (dotimes (i (- (length preample) 3))
                  (write-char #\space *trace-output*)))
              (write-string x *trace-output*)
              (terpri *trace-output*))))))))

;;; ---------------------------------------------------------------------------
;;;  SGML lexer

(defun read-token (input dtd)
  ;; Reads on token from the stream `input'
  ;; Returns:
  ;;    :pcdata      cdata
  ;;    :start-tag   name atts
  ;;    :end-tag     name
  ;;    :empty-tag   name atts
  ;;    :comment     cdata
  ;;    :define-tag  cdata
  ;;    :experimental-tag  cdata
  ;;    :eof
  (let ((ch (a-read-byte input)))
    (cond ((null ch)
           :eof)
          ((rune= ch #/<)
           (read-tag input dtd))
          (t
           (a-unread-byte ch input)
           (read-pcdata input dtd)) )))

(defun push-on-scratch (input sp ch)
  ;; Push the character ch onto the scratch pad of `input' and enlarge if neccessary
  (setf (aref (a-stream-scratch input) sp) ch)
  (setf sp (+ sp 1))
  (cond ((= sp (length (a-stream-scratch input)))       ;end of scratch pad reached?
         (enlarge-scratch-pad input)))
  sp)

(defun read-pcdata (input dtd)
  (let* ((scratch (a-stream-scratch input))             ;scratch pad
         (sp 0)                                         ;pointer into scratch pad
         (se (length scratch))                          ;end of scratch pad
         )             ;code vector
    (declare (type (simple-array rune (*)) scratch))
    (declare (type fixnum sp se))
    (loop
      (let ((ch (a-read-byte input)))
	;; FIXME: why was this declared as (u-b 8), not (u-b 16)?
	;; a-read-byte returns a rune.
;;; 	(declare (type (or null (unsigned-byte 8)) ch))
        (declare (type (or null rune) ch))
        (cond ((null ch)                                ;eof
               (return))
              ((rune= ch #/<)                 ;end of pcdata
               (a-unread-byte ch input)
               (return))
              ((rune= ch #/&)
               (setf sp (read-entity-ref input dtd sp))
               (if (>= sp se)  ;read-entity-ref may enlarge scratchpad
                 (setf scratch (a-stream-scratch input)
                       se (length scratch))))
              (t
               (setf (aref scratch sp) ch)    ;recode character read
               (setf sp (the fixnum (+ sp 1)))
               (cond ((= sp se)                         ;end of scratch pad reached?
                      (enlarge-scratch-pad input)
                      (setf scratch (a-stream-scratch input)
                            se (length scratch))))))))
    (values :pcdata
            (subseq/rod scratch 0 sp)) ))

(defun read-entity-ref (input dtd sp)
  ;; Reads an entity reference into the stream's scratch pad from position
  ;; `sp' upwards.
  ;; Returns the new write pointer. The initial "&" is already read from the
  ;; stream.
  ;; Syntax:
  ;;    entity-ref ::= "&" "#" <digit>+ (";")?
  ;;    entity-ref ::= "&" "#" "x" <hex-digit>+ (";")?
  ;;    entity-ref ::= "&" <name-start> <name-char>* (";")?
  (let ((ch (a-read-byte input)))
    (cond ((null ch)                                    ;eof
           (parse-warn input 3 "EOF in entity")
           (push-on-scratch input sp #/&))
          ((rune= ch #/#)				;numeric reference?
           (read-numeric-entity input sp))
          ((name-start-rune-p ch)                       ;named entity?
           (read-named-entity input dtd sp ch))
          (t
           (parse-warn input 3
                       "Saw character '~A' after '&' -- bad entity reference?!" 
                       (or (rune-char ch)
                           (format nil "&#x~4,'0X" ch)))
           (a-unread-byte ch input)     ;it might be something interesting
           (push-on-scratch input sp #/&)) )))

(defun read-numeric-entity (input sp)
  ;; "&#" already read
  (let ((ch (a-read-byte input)))
    (setf sp (push-on-scratch input sp #/&))
    (setf sp (push-on-scratch input sp #/#))
    (cond ((null ch)                                    ;eof
           (parse-warn input 3 "EOF in entity")
           sp)
          
          ((digit-rune-p ch)
           (read-numeric-entity-aux input (- sp 2) sp 10 ch))

          ((rune= ch #/x)
           ;; Hex entity
           (setf sp (push-on-scratch input sp #/x))
           (setf ch (a-read-byte input))
           (cond ((null ch)
                  (parse-warn input 3 "EOF after '&#x'.")
                  sp)
                 ((not (digit-rune-p ch 16))
                  (parse-warn input 3 "Bad character after '&#x'.")
                  (a-unread-byte ch input)
                  sp)
                 (t
                  (read-numeric-entity-aux input (- sp 3) sp 16 ch))))
          (t
           (a-unread-byte ch input)
           (parse-warn input 3 "Bad character after '&#'")
           sp) )))

(defun read-numeric-entity-aux (input s0 sp radix ch)
  ;; Aux routine for read-numeric-entity
  ;; at s0..sp in the scratch pad is the already read prefix ('&#' or '&#x')
  ;; Radix is the radix to use (10 or 16)
  ;; returns new 'sp'
  ;; 'ch' is the first digit
  (let ((s1 sp))
    (setf sp (push-on-scratch input sp ch))
    (do ((ch (a-read-byte input) (a-read-byte input)))
        ((or (null ch) (not (digit-rune-p ch radix)))
         ;; Ok. [s1..sp) now is the digit sequence
         (let ((num (parse-integer (map 'string #'rune-char
                                        (subseq (a-stream-scratch input) s1 sp))
                                   :radix radix)))
           (cond ((<= 0 num #xFFFF)
                  ;; Proper entity value
                  (when (and (not (null ch)) (not (rune= ch #/\;)))
                    (a-unread-byte ch input))
                  ;; Rewind scratch pad to `s0' and push character `num'
                  (setf sp (push-on-scratch input s0 (code-rune num))))
                 (t
                  ;; num too large; emit warning and leave scratch pad alone
                  (when (not (null ch))
                    (a-unread-byte ch input))
                  (parse-warn input 3 "Numeric enity ~A does not fit into our 16-bit strings; -- ignored."
                              (rod-string (rod-subseq (a-stream-scratch input) s0 sp)))))))
      (setf sp (push-on-scratch input sp ch)))
    sp))

(defun read-named-entity (input dtd sp ch)
  ;; Just in case we want to leave the entity alone
  (let ((s0 sp))
    (setf sp (push-on-scratch input sp #/&))
    (let ((s1 sp))
      (setf sp (push-on-scratch input sp ch))
      (do ((ch (a-read-byte input) (a-read-byte input)))
          ((or (null ch) (not (name-rune-p ch)))
           ;; Ok. [s1..sp) now is the name, try to resolve it
           (let ((nums (find-named-entity dtd (subseq (a-stream-scratch input) s1 sp))))
             (cond ((not (null nums))
                    ;; Proper entity value
                    ;; Rewind scratch pad to `s0' and push characters in `nums'
                    (dotimes (i (length nums))
                      (setf sp (push-on-scratch input s0 (aref nums i))))
                    (when (and (not (null ch)) (not (rune= ch #/\;)))
                      (a-unread-byte ch input)))
                   (t
                    (when (not (null ch))
                      (a-unread-byte ch input))
                    (parse-warn input 3 "[~D] There is no such entity defined: ~A -- ignored."
                                (a-stream-position input)
                                (rod-string (rod-subseq (a-stream-scratch input) s0 sp)))))) )
        (setf sp (push-on-scratch input sp ch))))
    sp))

(defun find-named-entity (dtd fat-string)
  (let ((str (rod-string fat-string)))
    (let ((r (cdr (assoc str (sgml::dtd-entities dtd) :test #'string=))))
      r)))

(defun enlarge-scratch-pad (input)
  (let* ((old (a-stream-scratch input))
         (se (length old)))
    (declare (type fixnum se)
             (type (simple-array rune (*)) old))
    (let ((new (make-rod (+ (length (a-stream-scratch input)) *buf-size*))))
      (declare (type rod new))
      (do ((i (- se 1) (the fixnum (- i 1))))
          ((< i 0))
        (declare (type fixnum i))
        (setf (aref new i) (aref old i)))
      (setf (a-stream-scratch input) new))))

;;; ------------------------------------------------------------

;;
;;           tag ::= <start-tag> | <end-tag> | <exp-tag> | <comment>
;;       end-tag ::= "<" "/" <name> WSP ">"
;;     empty-tag ::= "<" <name> <atts> WSP "/" ">"
;;     start-tag ::= "<" <name> <atts> WSP ">"
;;       exp-tag ::= "<" "?" <any>* ">"
;;       comment ::= "<" "!" "-" "-" (<any>* - ("-" "-")) "-" "-" ">"
;;                 | "<" "!" ">"
;;           att ::= <value> 
;;                 | <name> WSP "=" WSP <value>
;;         value ::= <literal> | <name>
;;       literal ::= """ <char>* """
;;                 | "'" <char>* "'"
;;         atts ::= ( WSP <att> )*
;;          WSP ::= <white-space>*
;;         name ::= <name-start-char> <name-char>*
;;         char ::= <any> | <enitity-ref>

(defun read-tag (input dtd)
  ;; The "<" is already read.
  (catch 'read-tag-error
    (let ((ch (a-peek-byte input)))
      (cond ((rune= ch #//)
             (a-read-byte input)
             (read-end-tag input))
            ((rune= ch #/!)
             (a-read-byte input)
             (read-define-tag input dtd))
            ((rune= ch #/?)
             (a-read-byte input)
             (read-experimental-tag input))
            ((and (not (null ch)) (name-start-rune-p ch))
             (read-start-tag input dtd))
            (t
             (parse-warn input 3 "Bad character after '<': '~A' -- ignored."
                         (rune-char ch))
             (let ((res (string-rod "<")))
               (values :pcdata res))) ))) )

(defun read-start-tag (input dtd)
  (multiple-value-bind (name atts) (read-name-and-attributes input dtd)
    (let ((ch (a-read-byte input)))
      (cond ((null ch)
            (read-tag-error input "EOF inside tag"))
           ((rune= ch #/>)
             (values :start-tag name atts))
            ((rune= ch #/<)
             (parse-warn input 3 
                         "A '<' ended this tag.")
             (a-unread-byte ch input)
             (values :start-tag name atts))
            ((rune= ch #//)
             (setf ch (a-read-byte input))
             (cond ((rune= ch #/>)
                    (values :empty-tag name atts))
                   (t
                    (read-tag-error input "Expected '>' after '<' .. '/'"))))
            (t
             (read-tag-error input "Expected '>'")) ))))

(defun read-end-tag (input)
  (let ((name (read-name input)))
    (skip-white-space input)
    (let ((ch (a-read-byte input)))
      (cond ((null ch)
             (read-tag-error input "In end tag: Expected '>' got end-of-file instead."))
            ((rune= ch #/>)
             (values :end-tag name))
            (t
             (read-tag-error input "In end tag: Expected '>'")) ))))

(defun read-name-and-attributes (input dtd)
  (let ((name (read-name input))
        (atts nil))
    (loop
      (skip-white-space input)
      (cond ((member (a-peek-byte input) '(#/< #/> #//) :test #'eql)
             (return)))
      (push (read-attribute input dtd) atts))
    (values name (nreverse atts)) ))

(defun read-name (input)
  (let ((ch (a-peek-byte input))
        (sp 0))
    (cond ((and (not (null ch)) (name-start-rune-p ch))
           (do ((ch (a-read-byte input) (a-read-byte input)))
               ((not (and ch (name-rune-p ch)))
                (when ch
                  (a-unread-byte ch input))
                (subseq/rod (a-stream-scratch input) 0 sp))
             (setf sp (push-on-scratch input sp (upcase-name-rune ch)))))
          (t
           (read-tag-error input "Not a name")) )))

(defun skip-white-space (input)
  (do ((ch (a-read-byte input) (a-read-byte input)))
      ((not (and ch (white-space-rune-p ch)))
       (when ch (a-unread-byte ch input)))))

(defun read-attribute (input dtd)
  (skip-white-space input)
  (let ((slot (read-sloopy-name input)))
    ;;(print (list 'slot '= (mungle slot) (mungle (vector (a-peek-byte input)))))
    (skip-white-space input)
    (let ((c (a-peek-byte input)))
      (cond ((and (not (null c)) (rune= c #/=))
             (a-read-byte input)
             (skip-white-space input)
             (let ((value (read-value input dtd)))
               (cons slot value)))
            (t
             slot)))))

(defun read-value (input dtd)
  (let ((ch (a-peek-byte input)))
    (cond ((rune= ch #/')
           (a-read-byte input)
           (read-literal input dtd ch))
          ((rune= ch #/\")
           (a-read-byte input)
           (read-literal input dtd ch))
          ((and ch (sloopy-name-rune-p ch))
           (read-sloopy-value input))
          (t
           (read-tag-error input "Bad value '~A' seen"
                           (or (rune-char ch)
			       (format nil "U+~4,'0X" (rune-code ch))))))))

(defun read-literal (input dtd delim)
  (let* ((scratch (a-stream-scratch input))             ;scratch pad
         (sp 0)                                         ;pointer into scratch pad
         (se (length scratch))                          ;end of scratch pad
         )             ;code vector
    (declare (type rod scratch))
    (declare (type fixnum sp se))
    (loop
      (let ((ch (a-read-byte input)))
	;; FIXME: why was this declared as (u-b 8), not (u-b 16)?
	;; a-read-byte returns a rune.
;;; 	(declare (type (or null (unsigned-byte 8)) ch))
        (declare (type (or null rune) ch))
        (cond ((null ch)                                ;eof
               (read-tag-error input "Eof in literal"))
              ((rune= ch delim)
               (return))
              ((rune= ch #/&)
               (setf sp (read-entity-ref input dtd sp)))
              (t
               (setf (aref scratch sp) ch)    ;recode character read
               (setf sp (the fixnum (+ sp 1)))
               (cond ((= sp se)                         ;end of scratch pad reached?
                      (enlarge-scratch-pad input)
                      (setf scratch (a-stream-scratch input)
                            se (length scratch))))))))
    (subseq/rod scratch 0 sp) ))
          
(defun read-sloopy-name (input)
  (let ((ch (a-peek-byte input))
        (sp 0))
    (cond ((and (not (null ch)) (sloopy-name-rune-p ch))
           (do ((ch (a-read-byte input) (a-read-byte input)))
               ((not (and ch (sloopy-name-rune-p ch)))
                (when ch
                  (a-unread-byte ch input))
                (subseq/rod (a-stream-scratch input) 0 sp))
             (setf sp (push-on-scratch input sp ch))))
          (t
           (read-tag-error input 
                           "Expected sloopy name, got ~A"
                           (or (rune-char ch) (format nil "U+~4,'0X" ch)) )) )))

(defun read-sloopy-value (input)
  (let ((ch (a-peek-byte input))
        (sp 0))
    (cond ((and (not (null ch)) (sloopy-value-rune-p ch))
           (do ((ch (a-read-byte input) (a-read-byte input)))
               ((not (and ch (sloopy-value-rune-p ch)))
                (when ch
                  (a-unread-byte ch input))
                (subseq/rod (a-stream-scratch input) 0 sp))
             (setf sp (push-on-scratch input sp ch))))
          (t
           (read-tag-error input "Expected sloopy value, got ~A"
                           (or (rune-char ch) (format nil "U+~4,'0X" ch)) )) )))

(defun read-define-tag (input dtd)
  (let ((ch (a-peek-byte input)))
    (cond ((null ch)
           (read-tag-error input "unexpected EOF"))
          ((rune= ch #/>)
           ;; empty define tag -- to be ignored
           (a-read-byte input)
           (read-token input dtd))
          ((rune= ch #/-)
           ;; comment?
           (a-read-byte input)
           (let ((ch (a-peek-byte input)))
             (cond ((and (not (null ch)) (rune= ch #/-))
                    (read-comment input))
                   (t
                    (read-tag-error input "Expected '-' after \"<!-\"")))))
          (t
           (read-define-tag-2 input)) )))

(defun read-define-tag-2 (input)
  ;; TODO: Comments
  ;; we simply slurp until '>'
  (let ((sp 0))
    (do ((ch (a-read-byte input) (a-read-byte input)))
        ((and ch (rune= ch #/>))
         (values :define-tag (subseq/rod (a-stream-scratch input) 0 sp)))
      (setf sp (push-on-scratch input sp ch))) ))

(defun read-comment (input)
  (a-read-byte input)   ;consume the '-'
  (let ((c0 0)
        (c1 (or (a-read-byte input) (read-tag-error input "Unexpected EOF")))
        (c2 (or (a-read-byte input) (read-tag-error input "Unexpected EOF")))
        (sp 0)
        (warned-p nil))
    (loop
      (psetq c0 c1
             c1 c2
             c2 (a-read-byte input))
      (cond ((null c2)
             (read-tag-error input "EOF within comment."))
            ((and (rune= c0 #/-)
                  (rune= c1 #/-)
                  (rune= c2 #/>))
             (return))
            ((and *gt-ends-comment-p* 
                  (rune= c2 #/>))
             (parse-warn input 3 "A '>' ends this comment.")
             (return)))
      (cond ((and (rune= c0 #/-) (rune= c1 #/-))
             (unless warned-p
               (parse-warn input 4 "\"--\" seen within comment; This is strongly depreciated.")
               (setf warned-p t))))
      (setf sp (push-on-scratch input sp c0)))
    (values :comment (subseq/rod (a-stream-scratch input) 0 sp)) ))

;;;; ------------------------------------------------------------------------------------------

(defun name-start-char-p (ch)
  (alpha-char-p ch))

(defun name-char-p (ch)
  (or (alphanumericp ch) (char= ch #\.) (char= ch #\-)) )

(defun valid-name-string-p (string)
  "Is the string `string' a valid name string according to the SGML
   conventions?"
  (and (> (length string) 0)
       (name-start-char-p (char string 0))
       (every #'name-char-p string)) )

;;;; ------------------------------------------------------------------------------------------
;;;;  Resolving Entities
;;;;

;;;; TODO: Check that numeric entities are within 0..#xFFFF;

(defun resolve-numeric-entity (string start end) ; --> string ; new start
  (let ((j (or (position-if-not #'digit-rune-p string :start start :end end) end)))
    (values 
     (let ((n (parse-integer (rod-string (rod-subseq string start j)) :radix 10)))
       (rod n))
     (if (and (< j end) (rune= (rune string j) #/\;))
         (+ j 1)
       j))))

(defun resolve-hex-entity (string start end) ; --> string ; new start
  ;; Resolves a hexadecimal entity like "&#x2A;", start should point
  ;; to the character directy after the '&#x'.
  (let ((j (or (position-if-not (rcurry #'digit-rune-p 16) string :start start :end end) end)))
    (values 
     (let ((n (parse-integer (rod-string (rod-subseq string start j)) :radix 16)))
       (rod n))
     (if (and (< j end) (rune= (rune string j) #/\;))
         (+ j 1)
       j))))

(defun resolve-named-entity (string entities start end &optional input)
  ;; --> string ; new start
  (let ((j (or (position-if-not #'name-start-rune-p string :start start :end end) end)))
    (let ((res 
           (or (dolist (k entities)
                 (when (and (= (length (car k)) (- j start))
			    ;; XXX this compare conses far too much!
			    (rod= (string-rod (subseq (car k) 0 (- j start)))
				  (rod-subseq string start j)))
                   (return (string-rod (cdr k))) ) )
               (dolist (k entities nil)
                 (when (and (>= (length (car k)) (- j start))
			    ;; XXX dito
			    (rod= (string-rod (subseq (car k) 0 (- j start)))
				  (rod-subseq string start j)))
                   (return (string-rod (cdr k))) )))))
      (cond ((not (null res))
	     (values 
	      (resolve-entities-in-string res entities 0 (length res) input)		;right?
	      (if (and (< j end) (rune= (rune string j) #/\;))
		  (+ j 1)
		j)))
	    (t
	     (parse-warn input 3 "Entity &~a; is not defined." (subseq string start j))
	     (values (subseq string (1- start) j) j))))))

(defun resolve-entities-in-string (string entities 
                                   &optional (start 0) (end (length string)) input)
  ;; Resolve all entity references introduced by "&" in the string
  ;; `string'.  `start' and `end' specify a substring to operate on.
  ;; For error messages `input' may be the input stream the data is
  ;; coming from originally.
  (let ((i (position #/& string :start start :end end :test #'rune=)))
    (cond ((null i)
           ;; no further entities in string -- all done
	   (rod-subseq string start end))
	  ((and (< (+ i 2) end) 
		(rune= #/# (rune string (+ i 1)))
		(digit-rune-p (rune string (+ i 2))))
	   ;; numeric entity seen
	   (multiple-value-bind (res j) (resolve-numeric-entity string (+ i 2) end)
	     (concatenate 'rod (subseq string start i)
			  res
			  (resolve-entities-in-string string entities j end input))))
	  ((and (< (+ i 2) end) 
		(rune= #/# (rune string (+ i 1)))
		(rune= #/x (rune string (+ i 2))))
	   ;; hexadecimal entity seen
	   (multiple-value-bind (res j) (resolve-hex-entity string (+ i 3) end)
	     (concatenate 'rod (subseq string start i)
			  res
			  (resolve-entities-in-string string entities j end input))))
	  ((and (< (+ i 1) end)
		(alpha-rune-p (aref string (+ i 1))))
           ;; this must be a named entity
	   (multiple-value-bind (res j) 
               (resolve-named-entity string entities (+ i 1) end input)
	     (concatenate 'rod (subseq string start i)
			  res
			  (resolve-entities-in-string string entities j end input))))
	  (t
           ;; no entity reference.
	   (concatenate 'rod (subseq string start i)
			(rod #/&)
			(resolve-entities-in-string string entities (+ i 1) end input))))))


;;;; ------------------------------------------------------------------------------------------
;;;;  Mungling of Attribute values
;;;;

(let ((kw-pkg (find-package :keyword)))
  (defun kintern (x)
    (intern x kw-pkg)))

(defun canon-value (input dtd tag slot value)
  (let* ((attlist (find-element-attlist dtd tag))
	 (looked  (assoc slot attlist)))
    (cond ((and looked (listp (cadr looked)))
	   (or (find value (cadr looked) 
                     :test #'(lambda (x y) 
                               (string-equal (string x) (string y))))
	       (progn
		 ;; Oh yeah! monster format strings are fun!
		 (parse-warn input 3 
			     "~S is a bad value for the '~A' slot of '<~A>', which could ~
                              ~{~#[not take any value~;only take '~A'~:;take one of ~@{'~A'~#[~; or ~:;, ~]~}~]~:}."
			     value slot tag (cadr looked)))))
	  ((member (cadr looked) '(:number))
           (or (maybe-parse-integer value)
               (progn 
                 (parse-warn input 3 "~S is not NUMBER (attribute '~A' of '<~A>')."
                             value slot tag)
                 nil)))
	  ((member (cadr looked) '(:name :id))
	   (cond ((valid-name-string-p value)
		  (kintern (string-upcase value)))
		 (t
		 (parse-warn input 3 "~S is not NAME (attribute '~A' of '<~A>')."
                             value slot tag)
		  nil)))
	  (looked
	   value)
	  (t
	   (parse-warn input 3 "The '<~A>' element has no '~A' slot." tag slot)
	   nil) )))

(defun find-slot-value-pair (input dtd tag value)
  (let* ((attlist (find-element-attlist dtd tag))
	 (looked  nil))
    (dolist (att attlist)
      (cond ((and (listp (cadr att))
		  (setq looked (find value (cadr att) 
                                     :test #'(lambda (x y) 
                                               (string-equal (string x) (string y))))))
	     (return-from find-slot-value-pair (values (car att) looked)))))
    ;;fall thru'
    (parse-warn input 3
		"The '<~A>' tag has no slot which could take the '~A' keyword.~%~
                 ~1{~#[There are no possible slots at all.~;~
                       Only possible slot is:~:;~
                       Possible slots are:~]~
                    ~@{~&  ~1{Slot '~A'~20T could ~{~#[not take any value~;~
                                                       only take '~A'~:;~
                                                       take one of ~@{'~A'~#[~; or ~:;, ~]~}~].~:}~}~}~:}"
		tag value (remove-if-not #'(lambda (x) (consp (cadr x))) attlist)) ))


;;;; ------------------------------------------------------------

(defun read-token* (input dtd)
  ;;(skip-white-space input)
  (multiple-value-bind (kind a b) (read-token input dtd)
    (ecase kind
      (:pcdata     (make-start-tag :name :pcdata :atts a))
      (:start-tag  
       (let ((name (kintern (rod-string a))))
         (if (tag-exists? dtd name)
             (make-start-tag :name name :atts (mungle-attlist dtd name b))
           (progn
             (parse-warn input 4 "There is no such thing as <~A> -- ignored." name)
             (read-token* input dtd)))))
      (:end-tag    
       (let ((name (kintern (rod-string a))))
         (if (tag-exists? dtd name)
             (make-end-tag :name name)
           (progn
             (parse-warn input 4 "There is no such thing as </~A> -- ignored." name)
             (read-token* input dtd)))) )
      (:empty-tag
       (parse-warn input 2 "Oops -- there is an empty tag; but this is only HTML?!")
       (let ((name (kintern (rod-string a))))
         (if (tag-exists? dtd name)
             (make-start-tag :name name :atts (mungle-attlist dtd name b))
           (progn
             (parse-warn input 4 "There is no such thing as <~A> -- ignored." name)
             (read-token* input dtd))))
       ;;(read-token* input dtd)
       )
      (:define-tag (read-token* input dtd))
      (:experimental-tag
       (parse-warn input 2 "Ignoreing processing instruction tag: '~A'" (mungle a))
       (read-token* input dtd))
      (:comment    
       (make-comment-token :data a)) 
      (:eof 
       (make-end-tag :name :%top)) )))

(defun tag-exists? (dtd name)
  (and 
   ;;(not (eq name :font))            ;xxx
   ;;(not (eq name :center))            ;xxx
   ;;(not (eq name :div))            ;xxx
   ;;(not (eq name :img))
   ;;(not (eq name :form))
   (not (eq name :noscript))            ;needed for www.sgi.com
   (sgml::find-element dtd name nil nil)))

(defun foofoo (r)
  (cond ((integerp r) (string-rod (prin1-to-string r)))
        ((symbolp r)  (string-rod (princ-to-string r)))
        ((stringp r)  (string-rod r))
        (t
         (error "foofoo: Hmm ~S ?!" r))))

;;; The renderer might depend on upper-case attribute values, so let's leave
;;; this off by default.  For the benefit of html <-> xml conversions we
;;; don't want to check the DTD every time we convert an attribute though,
;;; so we need this mode for lower-case attribute values.
(defvar *unmungle-attribute-case* nil)

(defun mungle-attlist (dtd tag atts)
  (mapcan (lambda (x)
            (cond ((atom x)
                   ;; this clause isn't unicode-safe
                   (multiple-value-bind (slot value) 
                       (sgml::find-slot-value-pair nil dtd tag (mungle x))
                     (when value
		       (setf value (foofoo value))
		       (when *unmungle-attribute-case*
			 (setf value (rod-downcase value)))) 
                     (and slot
                          (list slot value))))
                  (t
                   (let ((slot (kintern (string-upcase (mungle (car x))))))
                     (list slot (cdr x))))))
          atts))

(defun read-experimental-tag (input)
  ;; TODO: Comments
  ;; we simply slurp until '>'
  (let ((sp 0))
    (do ((ch (a-read-byte input) (a-read-byte input)))
        ((and ch (rune= ch #/>))
         (values :experimental-tag (subseq/rod (a-stream-scratch input) 0 sp)))
      (setf sp (push-on-scratch input sp ch))) ))


;;; ---------------------------------------------------------------------------
;;;  The PDA
;;;

(defun handle-meta-tag-in-parser (input attrs)
  (when (rod-equal (string-rod "content-type") (getf attrs :http-equiv))
    (let ((content-type (getf attrs :content)))
      (and content-type
           (multiple-value-bind (type subtype parameters)
               (closure-mime-types:parse-mime-content-type
		(rod-string content-type))
             (declare (ignore type subtype))
             (let ((cs (assoc :charset parameters :test #'string-equal)))
               (when cs
                 (setup-code-vector input (cdr cs)))))))))

(defun sgml-parse (dtd input)
  (let* ((stack (list (make-start-tag :name :%top :atts nil)))
         (s (sgml::make-pt/low :name 'top))
         (r s)
         (eof? nil)
         (eingabe nil)
         ausgabe)
    (loop
      (do ()
          ((or eof? (not (null (cdr eingabe)))))
        (let ((tok (read-token* input dtd)))
          (when (and (end-tag-p tok) (eq (tag-name tok) :%top))
            (setf eof? t))
          (setf eingabe (nconc eingabe (list tok)))))
      (multiple-value-setq (stack eingabe ausgabe) (transition input dtd stack eingabe))
      (cond ((eq ausgabe :accept)
             (return))
            
            ((eq ausgabe :error)
             (return))
            
            ((eq ausgabe :close)
             (setf s (sgml:pt-parent s)))

            ((eq (car ausgabe) :comment)
             (setf (sgml:pt-children s) 
               (nconc (sgml:pt-children s) (list (sgml::make-pt/low 
                                                  :name (cadr ausgabe)
                                                  :attrs (caddr ausgabe)
                                                  :children nil
                                                  :parent s)))))

            ((eq (car ausgabe) :open)
             ;; Hack here to support <meta http-equiv="Content-Type" ...>
             (cond ((and (eq (cadr ausgabe) :meta))
                    (handle-meta-tag-in-parser input (caddr ausgabe))))
             ;; when the BODY tag is openend, switch the streams speed to full speed.
             (cond ((and (eq (cadr ausgabe) :body))
                    (setf (runes::xstream-speed input)
                      (length (runes::xstream-os-buffer input)))))
             (let ((n (sgml::make-pt/low 
                       :name (cadr ausgabe)
                       :attrs (caddr ausgabe)
                       :children nil
                       :parent s)))
               (setf (sgml:pt-children s) (nconc (sgml:pt-children s) (list n))
                     s n) ))

            ((eq (car ausgabe) :open/close)
             ;; code duplication alert!
             ;; Hack here to support <meta http-equiv="Content-Type" ...>
             (cond ((and (eq (cadr ausgabe) :meta))
                    (handle-meta-tag-in-parser input (caddr ausgabe))))
             (let ((n (sgml::make-pt/low 
                       :name (cadr ausgabe)
                       :attrs (caddr ausgabe)
                       :children nil
                       :parent s)))
               (setf (sgml:pt-children s) (nconc (sgml:pt-children s) (list n))
                     s n) )
             (setf s (sgml:pt-parent s)))

            ((eq (car ausgabe) :htag)
             ;; code duplication alert!
             (let* ((v (cadr ausgabe))
                    (n (cond ((start-tag-p v)
                              (make-hstag-node
                               :name (tag-name v)
                               :attrs (start-tag-atts v)
                               :children nil
                               :parent s))
                             ((end-tag-p v)
                              (make-hetag-node
                               :name (tag-name v)
                               :attrs nil
                               :children nil
                               :parent s))
                             (t
                              (error "fix your code.")))))
               (setf (sgml:pt-children s) (nconc (sgml:pt-children s) (list n))) ))
            ))
    r) )

(defun empty-element-p (dtd gi)
  (null (elm-inclusion dtd gi)))

(defun transition (input dtd stack eingabe) ;; --> stack', eingabe', ausgabe
  (cond ((and stack (eq (tag-name (car stack)) :pcdata))
         (values (cdr stack)
                 eingabe
                 :close))
        ((null eingabe)
         (cond ((null stack)
                (values nil nil :accept))
               (t
                (values stack eingabe :error))))
        ;; (aS, </a>W) ->  (S, W, </a>)
        ((comment-token-p (car eingabe))
         (values stack
                 (cdr eingabe)
                 (if (member (tag-name (car stack)) *preserves-comments-elements*)
                     (progn
                       (list :comment :pcdata (comment-token-data (car eingabe))))
                   nil)))

        ((and (tag-p (car eingabe))
              (tag-as-marker-p (tag-name (car eingabe))))
         (values stack 
                 (cdr eingabe) 
                 (list :htag (car eingabe))))
        
        ((and (end-tag-p (car eingabe))
              stack
              (eq (tag-name (car stack))
                  (tag-name (car eingabe))))
         (values (cdr stack)
                 (cdr eingabe)
                 :close))
        
        ((and (start-tag-p (car eingabe))
              stack
              (member (tag-name (car eingabe)) (elm-inclusion dtd (tag-name (car stack)))) )
         (cond 
               ((empty-element-p dtd (tag-name (car eingabe)))
                (values stack
                        (cdr eingabe)
                        (list :open/close (tag-name (car eingabe)) (start-tag-atts (car eingabe)))))
               (t
                (values (cons (car eingabe) stack)
                        (cdr eingabe)
                        (list :open (tag-name (car eingabe)) (start-tag-atts (car eingabe)))))))
        ;;
        ((and (white-space-token-p (car eingabe))
              stack
              (not (member :pcdata (elm-inclusion dtd (tag-name (car stack))))))
         ;; ignorieren
         (values stack (cdr eingabe) nil))
        ((null stack)
         (error "Oops empty stack in TRANSITION on ~S." eingabe))
        (t
         (let ((x (resolve dtd (tag-name (car stack)) (car eingabe))))
           (if x
               (values stack
                       (cons x eingabe)
                       nil)
             (values stack 
                     (heuristic input dtd (car stack) eingabe) 
                     nil)))) ))

(defun tag-as-marker-p (gi)
  (and *font-heuristic-p*
       (eq gi :font))
  (and *anchor-heuristic-p*
       (eq gi :a)) )

(defun white-space-token-p (x)
  (and (start-tag-p x)
       (eq (tag-name x) :pcdata)
       (every #'white-space-rune-p (start-tag-atts x))))

;;; Heuristic conflict resolution

(defun shortest-different-beginning (x y)
  (do ((q x (cdr q))
       (i 0 (+ i 1)))
      ((null q))
    (do ((p y (cdr p))
         (j 0 (+ j 1)))
        ((null p))
      (if (eq p q)
          (return-from shortest-different-beginning
            (values (subseq x 0 i)
                    (subseq y 0 j)))))))

(defun document-action (input context alte-eingabe neue-eingabe is-default-p)
  (let ((see (car alte-eingabe)))
    (parse-warn input 4 "[~A] Saw ~A in ~A ~A"
                (if is-default-p "-" "H") see context
                (multiple-value-bind (a b) (shortest-different-beginning alte-eingabe neue-eingabe)
                  (cond ((and (null a) (null b))
                         (format nil "-- ??? patched ~S -> ~S" alte-eingabe neue-eingabe))
                        ((null b)
                         (format nil "-- nuked~{ ~A~}." a))
                        ((null a)
                         (format nil "-- inserted~{ ~A~}." b))
                        (t
                         (format nil "-- patched~{ ~A~} ->~{ ~A~}." a b)) )) )))

(defun heuristic (input dtd context eingabe)
  (let ((see (car eingabe))
        (is-default-p nil))
    (labels ((is (tag state)
               (and (elms-eqv dtd state (tag-name context))
                    (if (char= (char (symbol-name tag) 0) #\/)
                        (and (end-tag-p (car eingabe))
                             (elms-eqv dtd 
                                       (tag-name (car eingabe)) 
                                       (kintern (subseq (symbol-name tag) 1))))
                      (and (start-tag-p (car eingabe))
                           (elms-eqv dtd (tag-name (car eingabe)) tag))))))
      (let ((neu 
             (cond ((and (member :HTML (find-dtd-top-elements dtd))
                         (cond ((and (end-tag-p see) (eq (tag-name see) :%top))
                                (cons (elm-etag (tag-name context)) eingabe))

                               ((and (start-tag-p see)
                                     (eq (tag-name see) :style))
                                (cons (make-start-tag :name :SPAN
                                                      :atts (list :class (rod "illegalstyle")))
                                      (cdr eingabe)))

                               ((is :center #|in|# :h1)
                                (list* (elm-etag (tag-name context))
                                       (car eingabe) context (cdr eingabe)))
                               
                               #+(OR)
                               ;; this one for KMP
                               ((is :h2 #|in|# :a)
                                (list* (elm-etag (tag-name context)) (car eingabe) context (cdr eingabe)))
                               
                               ((or (is :center #|in|# :font) 
                                    (is :p #|in|# :font))
                                ;; Uff -- the attributes of FONT are lost here.
                                ;; we have to extend `context' to include these.
                                (list* (elm-etag (tag-name context)) (car eingabe) context (cdr eingabe)))
                               ((is :hr #|in|# :i)
                                (list* (elm-etag (tag-name context)) (car eingabe) context (cdr eingabe)))
                               ((elms-eqv dtd (tag-name context) ':font)
                                (cons (elm-etag (tag-name context))  eingabe))
                               ((is :tr #|in|# :center)
                                (cons (elm-etag (tag-name context)) eingabe))
                               ((is :/h2 #|in|# :h1)
                                (cons (elm-etag (tag-name context)) eingabe))
                               ((is :ul #|in|# :h1)
                                (cons (elm-etag (tag-name context)) eingabe))
                               ((is :/center #|in|# :h3)
                                (cons (elm-etag (tag-name context)) eingabe))
                               ((is :/td #|in|# :div)
                                (cons (elm-etag (tag-name context)) eingabe))
                               ((is :p #|in|# :ul)
                                (cons (make-start-tag :name :li) eingabe))
                               ((is :a #|in|# :ul)
                                (cons (make-start-tag :name :li) eingabe))
                               ((is :img #|in|# :ul)
                                (cons (make-start-tag :name :li) eingabe))
                               
                               ((is :pcdata #|in|# :ul)
                                (cons (make-start-tag :name :li) eingabe))

                               ((is :td #|in|# :table)
                                (list* (make-start-tag :name :tr)
                                       eingabe))
                               ((is :pcdata #|in|# :table)
                                (list* (elm-etag (tag-name context)) (car eingabe) context
                                       (cdr eingabe)))

                               ((is :frameset #|in|# :noscript)
                                (list* (elm-etag (tag-name context)) eingabe))

                               ((is :form #|in|# :table)
                                (list* (elm-etag (tag-name context))
                                       (car eingabe)
                                       context
                                       (cdr eingabe)))
                               
                               ((is :/form #|in|# :tbody)
                                ;; we should better check here wether FORM is open at all.
                                (list* (cadr eingabe)
                                       (car eingabe)
                                       (cddr eingabe)))
                               
                               ;; new as of 1999-08-31
                               ((is :td #|in|# :li)
                                (list* (elm-etag (tag-name context))
                                       eingabe))
                               ((is :/td #|in|# :li)
                                (list* (elm-etag (tag-name context))
                                       eingabe))
                               ((is :td #|in|# :ul)
                                (list* (elm-etag (tag-name context))
                                       eingabe))
                               ((is :/td #|in|# :ul)
                                (list* (elm-etag (tag-name context))
                                       eingabe))
                               
                               )))
                   (t
                    (setq is-default-p t)
                    (cdr eingabe)) )))
        (document-action input (tag-name context) eingabe neu is-default-p)
        neu))))

(defun parse-html (input &optional (charset :iso-8859-1))
  (let ((dtd closure-html:*html-dtd*))
    (let ((input (runes:make-xstream input :initial-speed 1 :speed 128)))
      (setf (a-stream-scratch input)
        (make-array #.(* 2 *buf-size*) :element-type 'rune))
      (setup-code-vector input charset)
      (let ((r (sgml-parse dtd input)))
        (post-mortem-heuristic dtd r)))) )

(defun post-mortem-heuristic (dtd parse-tree)
  "Do possible post mortem heuristic on a parse tree."
  (when *font-heuristic-p*
    (setf parse-tree (post-mortem/fix-font dtd parse-tree)))
  (setf parse-tree (post-mortem/fix-top-level-structure parse-tree)) 
  parse-tree)

(defun post-mortem/fix-top-level-structure (parse-tree)
  ;; The hacking below is needed because of buggy input. Something like this
  ;;  <base ..> <html> <head> .. <body> ... </html>
  ;; are two documents in one. Here we merge then into one HTML document.
  ;; Note that this defeats later incremental rendering pretty well.
  ;; Also this is HTML specific.
  ;; Das muss dennoch alles noch anders werden hier.
  (let ((r parse-tree)
        (head-elts nil)
        (body-elts nil)
        (frameset-elts nil)
        (head-warn-flag nil)
        (body-warn-flag nil)
        (frameset nil)
        (body nil))
    (when (> (length (pt-children r)) 1)
      (parse-warn nil 4 "Multiple HTML elements in document."))
    (dolist (k (pt-children r))
      (ecase (gi k)
        (:html
         (dolist (k (pt-children k))
           (ecase (gi k)
             (:head 
              (when head-elts
                (setf head-warn-flag t))
              (setf head-elts (nconc head-elts (pt-children k))))
             ((:body)
              (setq body k)
              (when body-elts
                (setf body-warn-flag t))
              (setf body-elts (nconc body-elts (pt-children k))))
             ((:frameset)
              (setq frameset k)
              (setf frameset-elts (nconc frameset-elts (pt-children k))) ))))))
    (when head-warn-flag
      (parse-warn nil 4 "Multiple HEAD elements."))
    (when body-warn-flag
      (parse-warn nil 4 "Multiple BODY elements."))
    (let* ((html (make-pt/low :name :html :parent nil))
           (head (make-pt/low :name :head :parent html :children head-elts)))
      (cond (frameset
             (cond (body
                    (parse-warn nil 4 "Body present while FRAMSET is present -- nuked!")
                    (setf body nil body-elts nil))
                   )
             (setf (pt-parent frameset) html
                   (pt-children frameset) frameset-elts)
             )
            (t
             (cond (body
                    (setf (pt-parent body) html
                          (pt-children body) body-elts))
                   (t
                    (setf body (make-pt/low :name :body 
                                            :parent html 
                                            :children body-elts))))))
      (dolist (k head-elts) (setf (pt-parent k) head))
      (dolist (k body-elts) (setf (pt-parent k) body))
      (dolist (k frameset-elts) (setf (pt-parent k) frameset))
      (setf (pt-children html) 
        (append (and head (list head))
                (and body (list body))
                (and frameset (list frameset))))
      html) ))

;;; FONT post mortem heuristic
;;; ==========================

;; Since FONT is by far the most often misused element, we handle these by a
;; post mortem analysis. If *font-heuristic-p* is true, parser does not care
;; for FONT start or end tags, but inserts HSTAG-NODEs and HETAG-NODEs into
;; the parse tree blindly. (Thus emulating the Mosaic approach to rendering).

;; Example
;; -------
;;
;;     <p> <font> foo <b> bar </font> baz </b>
;;
;; is then parsed as:
;;
;;     (P #<HSTAG font> "foo" (B "bar" #<HETAG font> "baz"))
;;

;; We now always want to return a parse tree, which conforms to the DTD, and
;; thus have to mungle this somehow into a sane HTML parse tree by wraping
;; FONT nodes around the right parts.

;; The first thing we do is to find all pairs of HSTAG, HETAG
;; nodes. We then use the following algorithm:

;; Algorithm
;; ---------

;; S = HSTAG node
;; E = HETAG node

;; if S and E are on the same level then  // [*] that is E, S have the same parent
;;    p <- S.parent
;;    ;; partitionate p.children as:
;;    p.children = (,@sb S ,@si E ,@se)
;;    if si = () then
;;       ;; FONT element spans nothing, so forget it 
;;    else
;;       if p may contain FONT and 
;;          for all x in si: FONT may contain x
;;       then
;;          p.children <- (,@sb (font ,@si) ,@se)
;;          all done
;;
;; if S is higher in tree than E then
;;    V <- ancestor of E, with V.parent = S.parent
;;    insert a copy of E directly before E
;;    insert a copy of S directly before first child of V
;;    apply the algorithm recursively
;;
;; if E is higher in tree than S then
;;    ;; this analog to the case above
;;    V <- ancestor of S, with V.parent = E.parent      // this ancestor need not to exist!
;;    insert a copy of S directly after V
;;    insert a copy of E direclly after last child of V
;;    apply the algorithm recursively
;;

;; [*] This is not right. This was an thinko: Two nodes on the same
;;     level do not always have the parent nodes. 

;; NOTE: My first formulation of the algorithm used access pathen and the
;; implementation below does also -- this is inefficient and should be
;; changed.

;; NOTE: It would be nice, if we could prove that switching on
;; *FONT-HEURISTIC-P* does not hurd any conforming document.

;; When the need araises, we could also use the same method to deal
;; malicious B, I, et al tags; But bad nesting isn't that popular any more
;; these days.

;; Expirience however showed, that we would need something similar for FORM,
;; since people have the habit of spitting a FORM anywhere they see
;; fit. [Most popular is right between TR and TD]. Visit www.deja.com and you
;; see what I mean. FORM elements are extremely important, since one may be
;; able to cope with gliberish on the screen, but not with non-functional
;; forms. Another thing here is <INPUT> elements in inlegal spots.

;;; TODO

;; We still want to emit parser warnings, if FONT elements are misused.

;;;(defun post-mortem/fix-font (dtd parse-tree)
;;;  (declare (special q))
;;;  (map-htag-pairs (lambda (stag etag)
;;;                    (let (s e)
;;;                      (setf s (pt-path parse-tree stag))
;;;                      (remove-pt stag)
;;;                      (setf e (pt-path parse-tree etag))
;;;                      (remove-pt etag)
;;;                      (mungle-font-pair dtd parse-tree stag s e)))
;;;                  parse-tree :font)
;;;  parse-tree)

(defun mungle-font-pair (dtd root tag s e)
  ;; NOTE: ignore-errors is needed, since ELM-INCLUSION checks for existing
  ;; element names (this is introduced by our pseudo TOP element).
  (cond ((equal s e))
        ((and (= (length s) (length e))
              (equal (butlast s) (butlast e)))
         ;; implicit assert was (butlast s) equal (butlast e), which is not always true.
         (let ((p (pt-access root (butlast s))))
           (cond ((and (member :font (ignore-errors (elm-inclusion dtd (gi p))))
                       (do ((j (car (last s)) (+ j 1)))
                           ((>= j (car (last e))) t)
                         (when (not (null (elt (pt-children p) j)))
                           ;; this should always be true!
                           (unless (member (gi (elt (pt-children p) j))
                                           (ignore-errors (elm-inclusion dtd :font)))
                             (return nil)))))
                  (let* ((before (loop for i from 0 to (1- (car (last s))) 
                                     collect (elt (pt-children p) i)))
                         (between (loop for i from (car (last s)) to (1- (car (last e)))
                                      collect (elt (pt-children p) i)))
                         (after (loop for i from (car (last e)) to (1- (length (pt-children p)))
                                    collect (elt (pt-children p) i)))
                         (new (make-pt/low :name (gi tag)
                                       :attrs (pt-attrs tag)
                                       :children between
                                       :parent p)))
                    (setf (pt-children p) (append before (list new) after))
                    (dolist (k between)
                      (setf (pt-parent k) new)) ))
                 (t
                  (loop for i from (car (last s)) to (1- (car (last e))) do
                        (mungle-font-pair
                         dtd root tag
                         (append (butlast s) (list i 0))
                         (append (butlast s) 
                                 (list i (length (pt-children (elt (pt-children p) i)))))))))))
        ((and (< (length s) (length e))
              '(equal (subseq e (length s)) s))
         (let ((v (subseq e 0 (length s))))
           (mungle-font-pair dtd root tag (append v (list 0)) e)
           (mungle-font-pair dtd root tag s v)))
        ((and (> (length s) (length e))
              '(equal (subseq s (length e)) e))
         (let* ((v (subseq s 0 (length e)))
                (m ))
           (mungle-font-pair dtd root tag
                             (append (butlast v) (list (+ 1 (car (last v))))) e)
           (setf m (length (pt-children (pt-access root v))))
           (mungle-font-pair dtd root tag
                             s (append v (list m)))))
        (t
         (let (p)
           ;; search to first common parent
           (do ((i 0 (+ i 1)))
               ((or (= i (length s))
                    (= i (length e))
                    (not (eq (elt s i) (elt e i))))
                (setf p (subseq s 0 i))))
           (let* ((v1 (append (subseq s 0 (length p))
                             (list (+ 1 (elt s (length p))))))
                  (v2 v1))
             (mungle-font-pair dtd root tag s v1)
             (mungle-font-pair dtd root tag v2 e))))
        ) )

(defun map-htag-pairs (continuation parse-tree gi)
  ;; this function maps the inner htags first.
  (let ((stack nil))
    (labels ((walk (x)
               (cond ((and (hstag-node-p x) (eq (gi x) gi))
                      (push x stack))
                     ((and (hetag-node-p x) (eq (gi x) gi))
                      (cond ((null stack)
                             (remove-pt x)
                             (parse-warn nil 4 "Superfluous ~A end tag." gi))
                            (t
                             (funcall continuation (pop stack) x))))
                     (t
                      (mapc #'walk (pt-children x))))))
      (walk parse-tree)
      ;; take care for non-closed start tags
      (unless (null stack)
        (parse-warn nil 4 "Unclosed ~A elements." gi)
        (let ((new (mapcar (lambda (x) 
                             (declare (ignore x))
                             (make-hetag-node :name gi
                                              :attrs nil
                                              :parent parse-tree
                                              :children nil))
                           stack)))
          (setf (pt-children parse-tree)
            (append (pt-children parse-tree) new))
          (mapc continuation stack new))) )))

;;; FORM post mortem heuristic
;;; ==========================

;; FORM is another element, which some authors get incredible
;; wrong. We deal with this in a similar way;

;; while we parse HTAG are put into the parse tree, we sort it out
;; after we are done parsing; There are two possibilities:

;; a. We are able to insert FORM in a sane way.
;; b. We are not able to do this

;; How is b. now handled? I see two solutions:

;;  - basically leave HTAGs in the tree.
;;  - permitt multiple FORM elements and connect them via some special
;;    attribute.

;;  

(defun setup-code-vector (input charset)
  (let ((enc (runes-encoding:find-encoding charset)))
    (cond ((not (null enc))
           (setf (runes:xstream-encoding input) enc))
          (t
           (parse-warn input 4 "There is no such encoding: ~S." charset)))))

(defun sgml-parse-file (filename dtd)
  (with-open-file (input filename :direction :input :element-type '(unsigned-byte 8))
    (sgml-parse dtd (make-a-stream :cl-stream (cl-byte-stream->gstream input)))))

(defun html-parse-file (filename)
  (with-open-file (input filename :direction :input :element-type '(unsigned-byte 8))
    (parse-html (cl-byte-stream->gstream input))))

#||
(defun html-parse-url (url)
  (unless (url:url-p url) (setf url (url:parse-url url)))
  (netlib:with-open-document ((input mime-type) url)
    mime-type
    (parse-html input)))
||#

(defun check-saneness (pt &optional (dtd closure-html:*html-dtd*))
  (dolist (k (pt-children pt))
    (unless (member (gi k) (elm-inclusion dtd (gi pt)))
      (warn "Unallowed ~A element within ~A." (gi k) (gi pt)))
    (unless (eq (pt-parent k) pt)
      (warn "Parent/child linkage broken."))
    (cond ((htag-node-p k)
           (warn "HTAG node (~S) left in parse tree. (parent ~S)" (gi k) (gi pt))))
    (check-saneness k)))

;;;(defun post-mortem/fix-font (dtd parse-tree)
;;;  ;; das hatten wir schon, so geht das nicht -- oder nur sehr schwer.
;;;  dtd
;;;  (let ((font-stack nil))               ;stack of open HTAG FONT elements
;;;    (labels ((walk (pt)
;;;               (cond ((hstag-node-p pt)
;;;                      (push pt font-stack)
;;;                      nil)
;;;                     ((hetag-node-p pt)
;;;                      (pop font-stack)
;;;                      nil)
;;;                     (t
;;;                      (cond ((not (null font-stack))
;;;                             ;; some fonts are open
;;;                             ;; warp them around this node
;;;                             
;;;                             )
;;;                            (t
;;;                             (let ((new-children (mapcan #'walk (pt-children pt))))
;;;                               (dolist (k new-children)
;;;                                 (setf (pt-parent k) pt))
;;;                               (setf (pt-children pt) new-children)
;;;                               (list pt))) )) )))
;;;      (car (walk parse-tree)) )))

(defun post-mortem/fix-font (dtd parse-tree)
  (declare (special q))
  (let ((pairs nil))
    ;; erstmal alle font tag paare suchen
    (map-htag-pairs (lambda (stag etag)
                      (push (cons stag etag) pairs))
                    parse-tree :font)
    (setf pairs (reverse pairs))
    ;; dann alle raus nehmen
    '(dolist (k pairs)
      (remove-pt (car k))
      (remove-pt (cdr k)))
    ;; dann alle munglen
    (dolist (pair pairs)
      (let ((stag (car pair)) (etag (cdr pair)) s e)
        (setf s (pt-path parse-tree stag))
        (assert (eq (pt-access parse-tree s) stag))
        (remove-pt stag)
        (setf e (pt-path parse-tree etag))
        (assert (eq (pt-access parse-tree e) etag))
        (remove-pt etag)
        (cond ((and s e)
               (mungle-font-pair dtd parse-tree stag s e))
              (t
               (warn "Hmm..."))))))
  parse-tree)

;;; ===========================================================================
;;;  'A' Heuristic
;;;

;; We want to match Netscape's behaviour and thus by experimentation,
;; we conclude:

(defun shortest-path-to (dtd pathen goal &optional (max-depth 10))
  (cond ((<= max-depth 0)
         nil)
        ((find-if (lambda (p)
                    (eql (car p) goal))
                  pathen))
        (t
         (shortest-path-to dtd
                           (mapcan (lambda (p)
                                     (mapcar (lambda (s)
                                               (cons s p))
                                             (elm-surclusion dtd (first p))))
                                   pathen)
                           goal
                           (1- max-depth))) ))

;;

#||
;; Start Tag anaylis

(defun blah (offending)
  (with-open-file (sink "/tmp/t.html" 
                   :direction :output
                   :if-exists :new-version)
    (let ((dtd closure-html:*html-dtd*))
      (let ((p (shortest-path dtd :BODY offending)))
        (let ((p2 (shortest-path dtd offending :PCDATA)))
          (format sink "~A<BR>~%" offending)
          (when (and p p2)
            (pop p)                     ;forget BODY
            (pop p2)
            (setq p2 (butlast p2))
            ;; pre-material
            (dolist (k (butlast p)) (format sink "<~A>~%" k))
            (format sink "<A href='xxx'>~%")
            (format sink "<img src='/tmp/a.gif'>A")
            (format sink "<~A>~%" (car (last p)))
            (dolist (k p2) (format sink "<~A>~%" k))
            (format sink "<img src='/tmp/b.gif'>B")
            (dolist (k (reverse p2)) (format sink "</~A>~%" k))
            (format sink "</~A>~%" (car (last p)))
            (format sink "<img src='/tmp/c.gif'>C")
            (dolist (k (reverse (butlast p))) (format sink "</~A>~%" k))
            ))))))

(defun open-in-netscape (url)
  (glisp:run-unix-shell-command 
   (format nil "my-netscape -remote 'openURL(~A)'" url)))

(defun bluu ()
  (let ((i 0))
    (dolist (off (all-elms closure-html:*html-dtd*))
      (cond (t '(or (member :B (elm-inclusion closure-html:*html-dtd* off))
                 (member :P (elm-inclusion closure-html:*html-dtd* off)))
             (blah off)
             (format T "~&;; ~A" off)
             (open-in-netscape "file:/tmp/t.html")
             (sleep 1)
             (let ((nam (format nil "/tmp/~A.gif" off)))
               (glisp:run-unix-shell-command (format nil "(my-xwd -id 0x3c003bf | xwdtopnm | ppmtogif) 2>/dev/null > ~A &" nam)))
             (sleep 1))
            (t
             (format T "~&;; Skipping ~A, because inclusion is ~A."
                     off (elm-inclusion closure-html:*html-dtd* off)))))))
||#

(defun equivalence-classes (prediate set)
  (let ((classes nil))
    (dolist (item set)
      (do ((cp classes (cdr cp)))
          ((null cp)
           (push (list item) classes))
        (cond ((funcall prediate (caar cp) item)
               (push item (car cp))
               (return)))))
    classes))

(defun shortest-path (dtd from to &optional (max-depth 10))
  (reverse (shortest-path/aux dtd (list (list from)) to max-depth)))

(defun shortest-path/aux (dtd pathen to &optional (max-depth 10))
  (cond ((<= max-depth 0)
         nil)
        ((null pathen)
         nil)
        ((find-if (lambda (p) (eq (car p) to)) pathen))
        ((shortest-path/aux
          dtd
          (remove-duplicates 
           (mapcan (lambda (path)
                     (mapcar (lambda (child)
                               (cons child path))
                             (elm-inclusion dtd (car path))))
                   pathen)
           :test (lambda (x y) (eql (car x) (car y))))
          to 
          (1- max-depth)))))

#||
(defun bloo ()
  (let ((dtd closure-html:*html-dtd*))
    (equivalence-classes (lambda (x y)
                           (and (set-equal (elm-inclusion dtd x) (elm-inclusion dtd y))
                                (set-equal (elm-surclusion dtd x) (elm-surclusion dtd y))))
                         (all-elms dtd))))
||#

;;; ===========================================================================

(defun mungle (x)
  (cond ((and (vectorp x) (not (stringp x))) (rod-string x))
        ((atom x) x)
        ((cons (mungle (car x))
               (mungle (cdr x))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
    (format T "~&;; Parse error (line ~D column ~D): [~A] Saw ~A in ~A."
            (runes:xstream-line-number input)
            (runes:xstream-column-number input))

||#
