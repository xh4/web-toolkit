;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/util.lisp,v 1.33 2012-05-04 21:17:44 edi Exp $

;;; Copyright (c) 2008-2012, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-unicode)

(defun parse-hex (string)
  "Parses STRING as a hexadecimal number."
  (parse-integer string :radix 16))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun canonicalize-name (name)
  "Converts the string NAME into a \"canonicalized\" name which can be
used for unambiguous look-ups by removing all whitespace, hyphens, and
underline characters.

Tries not to remove hyphens preceded by spaces if this could lead to
ambiguities as described in
<http://unicode.org/unicode/reports/tr18/#Name_Properties>.

All CL-UNICODE functions which accept string \"names\" for characters
or properties will canonicalize the name first using this function and
will then look up the name case-insensitively."
  (values (ppcre:regex-replace-all "[ _](-A|O-E)$|[-_\\s]" name
                                   (lambda (match register)
                                     (declare (ignore match))
                                     (cond (register (format nil " ~A" register))
                                           (t "")))
                                   :simple-calls t)))

(defun property-symbol (name)
  "Returns a symbol in the CL-UNICODE-NAMES package \(which is only
used for this purpose) which can stand in for the string NAME in
look-ups.  The symbol's name is the result of \"canonicalizing\" and
then upcasing NAME.

A symbol returned by this function is only really useful and only
actually a property symbol if the second return value is true.

All exported functions of CL-UNICODE which return strings which are
property names return the corresponding property symbol as their
second return value.  All exported functions of CL-UNICODE which
accept property names as arguments will also accept property symbols.

See also PROPERTY-NAME."
  (let ((symbol (intern (string-upcase (canonicalize-name name)) :cl-unicode-names)))
    (values symbol (property-name symbol))))

(defun register-property-symbol (name)
  "Converts NAME to a property symbol using PROPERTY-SYMBOL and
\"registers\" it in the *CANONICAL-NAMES* hash table."
  (let ((symbol (property-symbol name)))
    (setf (gethash symbol *canonical-names*) name)
    symbol))

(defun lookup-property-alias (name)
  "Returns the long-name of the given property alias"
  (gethash (string-upcase (canonicalize-name name)) *property-aliases*))

(defun property-name (symbol)
  "Returns a name \(not \"the\" name) for a property symbol SYMBOL if
it is known to CL-UNICODE.  Note that

  \(STRING= \(PROPERTY-NAME \(PROPERTY-SYMBOL <string>)) <string>)

is not necessarily true even if the property name is not NIL while

  \(EQ \(PROPERTY-SYMBOL \(PROPERTY-NAME <symbol>)) <symbol>)

always holds if there is a property name for <symbol>.

See also PROPERTY-SYMBOL."
  (values (gethash symbol *canonical-names*)))
) ;; END EVAL-WHEN

(defun tree-lookup (code-point tree)
  "Looks up an attribute for CODE-POINT in the binary search tree
TREE.  TREE is a tree as created by BUILD-TREE."
  (labels ((try (node)
             (and node
                  (destructuring-bind (((from . to) . value) left-branch right-branch)
                      node
                    (cond ((< code-point from) (try left-branch))
                          ((> code-point to) (try right-branch))
                          (t value))))))
    (try tree)))

(defgeneric mapping (c position want-code-point-p)
  (:documentation "Returns the simple case mapping for the character C
\(a code point or a Lisp character) in position POSITION where 0 means
lowercase, 1 uppercase, and 2 titlecase.  Returns a character if
WANT-CODE-POINT-P is NIL and a code point otherwise.")
  (:method ((char character) position want-code-point-p)
   (mapping (char-code char) position want-code-point-p))
  (:method ((code-point integer) position want-code-point-p)
   (let* ((mappings (gethash code-point *case-mappings*))
          (code-point (or (nth position mappings) code-point)))
     (if want-code-point-p
       code-point
       (and code-point (code-char code-point))))))

(defun cjk-unified-ideograph-p (code-point)
  "Returns a true value if CODE-POINT is the code point of a CJK
unified ideograph for which we can algorithmically derive the name."
  (or (<= #x3400 code-point #x4db5)
      (<= #x4e00 code-point #x9fc3)
      (<= #x20000 code-point #x2a6d6)))

(defun maybe-compute-cjk-name (code-point)
  "Computes the name for CODE-POINT if CODE-POINT denotes a CJK
unified ideograph the name of which can be algorithmically derived."
  (when (cjk-unified-ideograph-p code-point)
    (format nil "CJK UNIFIED IDEOGRAPH-~X" code-point)))

(defun maybe-find-cjk-code-point (name)
  "Computes the code point for NAME if NAME is the name of a CJK
unified ideograph the name of which can be algorithmically derived."
  (ppcre:register-groups-bind ((#'parse-hex code-point))
      ;; canonicalized
      ("(?i)^CJKUNIFIEDIDEOGRAPH([0-9A-F]{4,5}|10[0-9A-F]{4})$" name)
    (when (cjk-unified-ideograph-p code-point)
      code-point)))

(defmacro define-hangul-constant (name value)
  "Simple helper macro to define some constants needed for the Hangul
algorithm below."
  (flet ((create-symbol (name)
           (intern (format nil "+~:@(~C-~A~)+" (char name 0) (subseq name 1)) :cl-unicode)))
    ;; use EVAL-WHEN so the following definitions can refer to the
    ;; value already
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defconstant ,(create-symbol name) ,value
         ,(format nil "The constant `~A' from chapter 3 of the Unicode book." name)))))

(define-hangul-constant "SBase" #xac00)
(define-hangul-constant "LBase" #x1100)
(define-hangul-constant "VBase" #x1161)
(define-hangul-constant "TBase" #x11a7)
(define-hangul-constant "VCount" 21)
(define-hangul-constant "TCount" 28)
(define-hangul-constant "NCount" (* +V-COUNT+ +T-COUNT+))

(declaim (inline compute-hangul-name))
(defun compute-hangul-name (code-point)
  "Algorithmically derives the Hangul syllable name \(the part behind
\"HANGUL SYLLABLE \") of the character with code point CODE-POINT as
described in section 3.12 of the Unicode book."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum code-point))
  (let* ((s-index (- code-point +S-BASE+))
         (l-value (+ +L-BASE+ (floor s-index +N-COUNT+)))
         (v-value (+ +V-BASE+ (floor (mod s-index +N-COUNT+) +T-COUNT+)))
         (t-value (+ +T-BASE+ (mod s-index +T-COUNT+))))
    (declare (fixnum s-index t-value))
    (format nil "~A~A~@[~A~]"
            (gethash l-value *jamo-short-names*)
            (gethash v-value *jamo-short-names*)
            (and (/= t-value +T-BASE+)
                 (gethash t-value *jamo-short-names*)))))

(defconstant +first-hangul-syllable+ #xac00
  "The code point of the first Hangul syllable the name of which can
be algorithmically derived.")
(defconstant +last-hangul-syllable+ #xd7a3
  "The code point of the last Hangul syllable the name of which can be
algorithmically derived.")

(defun add-hangul-names ()
  "Computes the names for all Hangul syllables and registers them in
the *HANGUL-SYLLABLES-TO-CODE-POINTS* hash table.  Used for
CHARACTER-NAMED."
  (declare #.*standard-optimize-settings*)
  (when *compile-verbose*
    (format t "~&;;; Computing Hangul syllable names")
    (force-output))
  (loop for code-point from +first-hangul-syllable+ to +last-hangul-syllable+
        for name = (compute-hangul-name code-point)
        do (setf (gethash name *hangul-syllables-to-code-points*) code-point)))

(defun hangul-syllable-p (code-point)
  "Returns a true value if CODE-POINT is the code point of a Hangul
syllable for which we can algorithmically derive the name."
  (<= +first-hangul-syllable+ code-point +last-hangul-syllable+))

(defun maybe-compute-hangul-syllable-name (code-point)
  "Computes the name for CODE-POINT if CODE-POINT denotes a Hangul
syllable the name of which can be algorithmically derived."
  (when (hangul-syllable-p code-point)
    (format nil "HANGUL SYLLABLE ~X" (compute-hangul-name code-point))))

(defun maybe-find-hangul-syllable-code-point (name)
  "Computes the code point for NAME if NAME is the name of a Hangul
syllable the name of which can be algorithmically derived."
  (ppcre:register-groups-bind (name)
      ;; canonicalized
      ("(?i)^HANGULSYLLABLE([A-Z]*)$" name)
    (gethash name *hangul-syllables-to-code-points*)))

(defmacro ensure-code-point (c)
  "Helper macro so that C can be treated like a code point even if it
is a Lisp character."
  (with-rebinding (c)
    `(etypecase ,c
       (integer ,c)
       (character (char-code ,c)))))

