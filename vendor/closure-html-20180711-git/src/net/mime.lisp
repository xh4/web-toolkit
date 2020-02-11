;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: NETLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Simple-minded MIME handling
;;;   Created: 1997-04-12
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

(in-package :closure-mime-types)

(defstruct mime-type
  name          ;the name of the mime type, a string; e.g. "image/tiff"
  extensions)   ;a list of file name extensions, which qualify that mime type

(defvar *mime-types* nil
  "A simple list of all defined mime types")

(defun define-mime-type (name &key extensions)
  "Define or re-define a mime type."
  ;; delete old entry
  (setq *mime-types* (delete name *mime-types* :key #'mime-type-name :test #'string-equal))
  ;; add new entry
  (push (make-mime-type :name name :extensions extensions)
	*mime-types*)
  name)

(defun find-mime-type-from-extension (extension)
  "Given a file name extension find the corressponding mime type;
   returned is either NIL, if not found, or the mime-type object."
  (or
   (find extension *mime-types*
         :key #'mime-type-extensions
         :test #'(lambda (x y) (member x y :test #'string-equal)))
   ;; second try -- compare only the first three letters of the extension,
   ;; this is for supporting legacy file systems
   (find extension *mime-types* :key #'mime-type-extensions 
	 :test #'(lambda (x y) 
		   (member x y :test #'(lambda (x y)
					 (string-equal x y :end2 (min (length y) 3)))))) ))

(defun find-mime-type (name)
  "Finds a mime type by name, returns NIL, if not found, or the mime type object."
  (find name *mime-types* :key #'mime-type-name :test #'string-equal))


;;;; ------------------------------------------------------------------------------------------
;;;;  definition of mime types
;;;;

(define-mime-type "image/gif"		:extensions '("gif"))
(define-mime-type "image/jpeg"		:extensions '("jpeg" "jpg" "jpe"))
(define-mime-type "image/tiff"		:extensions '("tiff" "tif"))
(define-mime-type "image/png"		:extensions '("png"))
(define-mime-type "image/x-xpixmap"	:extensions '("xpm"))
(define-mime-type "image/x-xbitmap"	:extensions '("xbm"))
(define-mime-type "text/html"		:extensions '("html"))
(define-mime-type "text/lml"		:extensions '("lml"))
(define-mime-type "text/xml"		:extensions '("xml"))
(define-mime-type "text/plain"		:extensions '("txt"))
(define-mime-type "text/css"		:extensions '("css"))

;;;; -----------------------------------------------------------------------------------------

#|

     content  :=   "Content-Type"  ":"  type  "/"  subtype  *(";" parameter)
               ; case-insensitive matching of type and subtype
     type :=     "application"     / "audio"
               / "image"           / "message"
               / "multipart"       / "text"
               / "video"           / extension-token
               ; All values case-insensitive

     subtype := token ; case-insensitive

     parameter := attribute "=" value

     attribute := token   ; case-insensitive

     value := token / quoted-string
|#

(defun parse-mime-content-type (string &optional (start 0) (end (length string)))
  ;; Parses a 'Content-Type' field. Returns three values:
  ;;    type        ; a string
  ;;    subtype     ; a string
  ;;    parameters  ; an alist of attribute/value pairs (all strings)
  ;; *or*
  ;;    NIL         ; `string' does not parse into a 'Content-Type' field.
  ;;
  (multiple-value-bind (p type subtype) (parse-mime/type-and-subtype string start end)
    (and p (multiple-value-bind (p parameter-alist) (parse-mime/parameter-list string p end)
	     (and p (null (position-if-not #'white-space-p string :start p :end end))
		  (values type subtype parameter-alist))))))

(defun parse-mime/literal-char-p (ch)
  (find ch "!#$%&'*+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`abcdefghijklmnopqrstuvwxyz{|}~"))

(defun parse-mime/token (string &optional (start 0) (end (length string)))
  (let ((p0 (position-if-not #'white-space-p string :start start :end end)))
    (and p0 
	 (parse-mime/literal-char-p (char string p0))
	 (<= (+ p0 1) end)
	 (let ((p1 (or (position-if-not #'parse-mime/literal-char-p string
                                        :start (+ p0 1) :end end)
		       end)))
	   (values p1 (subseq string p0 p1))))))

(defun parse-mime/special (ch string &optional (start 0) (end (length string)))
  (let ((p (position-if-not #'white-space-p string :start start :end end)))
    (and p (char= ch (char string p))
	 (values (+ p 1)) )))

(defun parse-mime/type-and-subtype (string &optional (start 0) (end (length string)))
  (multiple-value-bind (p type) (parse-mime/token string start end)
    (and p (multiple-value-bind (p) (parse-mime/special #\/ string p end)
	   (and p (multiple-value-bind (p subtype) (parse-mime/token string p end)
		    (values p type subtype)))))))

(defun parse-mime/parameter (string &optional (start 0) (end (length string)))
  (multiple-value-bind (p attribute) (parse-mime/token string start end)
    (and p (multiple-value-bind (p) (parse-mime/special #\= string p end)
             (and p (multiple-value-bind (p value) 
                        (multiple-value-or (parse-mime/token string p end)
                                           (parse-mime/quoted-string string p end))
		    (and p (values p attribute value))))))))

(defun parse-mime/quoted-string (string &optional (start 0) (end (length string)))
  (let ((p (position-if-not #'white-space-p string :start start :end end)))
    (and (char= (char string p) #\")
	 (let ((res nil))
	   (do ((i (+ p 1) (+ i 1)))
	       ((or (= i end) (char= (char string i) #\"))
		(if (and (< i end) (char= (char string i) #\"))
		    (values (+ i 1) (coerce (nreverse res) 'string))
		  nil))
	     (if (and (char= (char string i) #\\) (< (+ i 1) end))
		 (progn 
		   (push (char string (+ i 1)) res)
		   (incf i))
	       (push (char string i) res)))))))

(defun parse-mime/parameter-list (string &optional (start 0) (end (length string)))
  (multiple-value-bind (p0) (parse-mime/special #\; string start end)
    (if p0
	(multiple-value-bind (p1 attribute value) (parse-mime/parameter string p0 end)
	  (and p1 (multiple-value-bind (p2 rest) (parse-mime/parameter-list string p1 end)
		    (and p2 (values p2 (cons (cons attribute value) rest))))))
      (values start nil) )))

(defun unparse-parameter-list (parameter-alist)
  ;; was ist mit ungewoehnlicheren Zeichen, wie NL ?!
  (format nil "~{; ~A=~S~}" (mapcan (lambda (x) (list (car x) (cdr x))) parameter-alist)))

(defun unparse-content-type (type subtype parameter-alist)
  (format nil "~A/~A~A" type subtype (unparse-parameter-list parameter-alist)))

(defun compose-multi-part-message (content-subtype parts)
  ;; all the parts are strings
  (labels ((invent-boundary ()
             (with-standard-io-syntax
               (write-to-string (random (expt 36 10)) :base 36)))
           (boundary-good-p (boundary)
             (not (some (lambda (part)
                          (search (concatenate 'string (string #\newline) "--" boundary) part))
                        parts)))
           (find-boundary ()
             (let ((x (invent-boundary)))
               (if (boundary-good-p x)
                   x
                 (find-boundary)))))
    (let ((boundary (find-boundary)))
      (with-output-to-string (sink)
        (format sink "Content-Type: ~A~%"
                (unparse-content-type :multipart content-subtype 
                                      (list (cons :boundary boundary) )))
        (format sink "Content-Transfer-Encoding: 8bit~%")
        (dolist (k parts)
          (format sink "~%--~A~%" boundary)
          (write-string k sink))
        (format sink "~%--~A--~%" boundary))) ))

(defun make-simple-disposition (name content)
  (with-output-to-string (sink)
    (format sink "Content-Disposition: form-data~A~%"
            (unparse-parameter-list (list (cons :name name))))
    (format sink "Content-Type: ~A~%"
            (unparse-content-type :text :plain '((:charset . "iso-8859-1"))))
    (format sink "~%")
    (write-string content sink)))

(defun compose-multipart/form-data (alist)
  (compose-multi-part-message
   :form-data (mapcar (lambda (x)
                        (make-simple-disposition (car x) (cdr x)))
                      alist)))

#|||
5.2.  Base64 Content-Transfer-Encoding

   The Base64 Content-Transfer-Encoding is designed to represent
   arbitrary sequences of octets in a form that need not be humanly
   readable.

   A 65-character subset of US-ASCII is used, enabling 6 bits to be
   represented per printable character. (The extra 65th character, "=",
   is used to signify a special processing function.)

   The encoding process represents 24-bit groups of input bits as output
   strings of 4 encoded characters. Proceeding from left to right, a
   24-bit input group is formed by concatenating 3 8-bit input groups.
   These 24 bits are then treated as 4 concatenated 6-bit groups, each
   of which is translated into a single digit in the base64 alphabet.
   When encoding a bit stream via the base64 encoding, the bit stream
   must be presumed to be ordered with the most-significant-bit first.

   That is, the first bit in the stream will be the high-order bit in
   the first byte, and the eighth bit will be the low-order bit in the
   first byte, and so on.

   Each 6-bit group is used as an index into an array of 64 printable
   characters. The character referenced by the index is placed in the
   output string. These characters, identified in Table 1, below, are
   selected so as to be universally representable, and the set excludes
   characters with particular significance to SMTP (e.g., ".", CR, LF)
   and to the encapsulation boundaries defined in this document (e.g.,
   "-").

   Alphabet: A-Z a-z 0-9 + /   and '=' for padding

   The output stream (encoded bytes) must be represented in lines of no
   more than 76 characters each.  All line breaks or other characters
   not found in Table 1 must be ignored by decoding software.  In base64
   data, characters other than those in Table 1, line breaks, and other
   white space probably indicate a transmission error, about which a
   warning message or even a message rejection might be appropriate
   under some circumstances.

   Special processing is performed if fewer than 24 bits are available
   at the end of the data being encoded.  A full encoding quantum is
   always completed at the end of a body.  When fewer than 24 input bits
   are available in an input group, zero bits are added (on the right)
   to form an integral number of 6-bit groups.  Padding at the end of
   the data is performed using the '=' character.  Since all base64
   input is an integral number of octets, only the following cases can
   arise: (1) the final quantum of encoding input is an integral
   multiple of 24 bits; here, the final unit of encoded output will be
   an integral multiple of 4 characters with no "=" padding, (2) the
   final quantum of encoding input is exactly 8 bits; here, the final
   unit of encoded output will be two characters followed by two "="
   padding characters, or (3) the final quantum of encoding input is
   exactly 16 bits; here, the final unit of encoded output will be three
   characters followed by one "=" padding character.

   Because it is used only for padding at the end of the data, the
   occurrence of any '=' characters may be taken as evidence that the
   end of the data has been reached (without truncation in transit).  No
   such assurance is possible, however, when the number of octets
   transmitted was a multiple of three.

   Any characters outside of the base64 alphabet are to be ignored in
   base64-encoded data.  The same applies to any illegal sequence of
   characters in the base64 encoding, such as "====="


|||#


(defvar *base64-codes* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun encode-string-base64 (string &key (start 0) (end (length string)))
  (let ((res nil))
    (do ((i start (+ i 3)))
        ((>= i end)
         (coerce (nreverse res) 'string))
      (let ((m (dpb (if (> (- end i) 2) (char-code (char string (+ i 2))) 0) (byte 8 0)
                    (dpb (if (> (- end i) 1) (char-code (char string (+ i 1))) 0) (byte 8 8) 
                         (dpb (char-code (char string i)) (byte 8 16) 
                              0)))))
        (push (aref *base64-codes* (+ 0 (ldb (byte 6 18) m))) res)
        (push (aref *base64-codes* (+ 0 (ldb (byte 6 12) m))) res)
        (push (if (> (- end i) 1) 
                  (aref *base64-codes* (+ 0 (ldb (byte 6 6) m)))
                #\=) 
              res)
        (push (if (> (- end i) 2) 
                  (aref *base64-codes* (+ 0 (ldb (byte 6 0) m)))
                #\=)
              res) ))))

(defun decode-base64-string (string &key (start 0) (end (length string)))
  (with-output-to-string (sink)
    (let ((ptr start))
      (labels ((read-code ()
                 (let (c)
                   (cond ((>= ptr end) '=)
                         ((setq c (position (char string ptr) *base64-codes* :test #'char=))
                          (incf ptr)
                          c)
                         ((char= (char string ptr) #\=) (incf ptr) '=)
                         (t (incf ptr) (read-code)))))
               (emit (byte)
                 (write-char (code-char byte) sink)))
        (loop
          (let ((c1 (read-code))
                (c2 (read-code))
                (c3 (read-code))
                (c4 (read-code)))
            (cond ((eq c1 '=) (return))
                  ((eq c2 '=) (return))
                  ((eq c3 '=) 
                   (emit (dpb c1 (byte 6 2) (ldb (byte 2 4) c2)))
                   (return))
                  ((eq c4 '=)
                   (emit (dpb c1 (byte 6 2) (ldb (byte 2 4) c2)))
                   (emit (dpb (ldb (byte 4 0) c2) (byte 4 4) (ldb (byte 4 2) c3)))
                   (return))
                  (t
                   (emit (dpb c1 (byte 6 2) (ldb (byte 2 4) c2)))
                   (emit (dpb (ldb (byte 4 0) c2) (byte 4 4) (ldb (byte 4 2) c3)))
                   (emit (dpb (ldb (byte 2 0) c3) (byte 2 6) c4))))))))))

;;;

(defun mime-type-equal (mime-type type subtype)
  (string-equal (mime-type-name mime-type)
                (format nil "~A/~A" type subtype)))

