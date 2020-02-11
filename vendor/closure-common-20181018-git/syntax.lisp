;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RUNES; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Unicode strings (called RODs)
;;;   Created: 1999-05-25 22:29
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: Lisp-LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

;;; This code is free software; you can redistribute it and/or modify it
;;; under the terms of the version 2.1 of the GNU Lesser General Public
;;; License as published by the Free Software Foundation, as clarified
;;; by the "Preamble to the Gnu Lesser General Public License" found in
;;; the file COPYING.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License is in the file
;;; COPYING that was distributed with this file.  If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt (until
;;; superseded by a newer version) or write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-08-15  GB      - ROD=, ROD-EQUAL
;;                        RUNE<=, RUNE>=
;;                        MAKE-ROD, ROD-SUBSEQ
;;                        CHAR-RUNE, RUNE-CHAR, ROD-STRING, STRING-ROD
;;                        new functions
;;                      - Added rune reader
;;

(in-package :runes)

;;;;
;;;; RUNE Reader
;;;;

;; Portable implementation of WHITE-SPACE-P with regard to the current
;; read table -- this is bit tricky.

(defun rt-white-space-p (char)
  (let ((stream (make-string-input-stream (string char))))
    (eq :eof (peek-char t stream nil :eof))))

(defun read-rune-name (input)
  ;; the first char is unconditionally read
  (let ((char0 (read-char input t nil t)))
    (when (char= char0 #\\)
      (setf char0 (read-char input t nil t)))
    (with-output-to-string (res)
      (write-char char0 res)
      (do ((ch (peek-char nil input nil :eof t) (peek-char nil input nil :eof t)))
          ((or (eq ch :eof)
               (rt-white-space-p ch)
               (multiple-value-bind (function non-terminating-p) (get-macro-character ch)
                 (and function (not non-terminating-p)))))
        (write-char ch res)
        (read-char input)))))           ;consume this character

(defun iso-10646-char-code (char)
  (char-code char))

(defvar *rune-names* (make-hash-table :test #'equal)
  "Hashtable, which maps all known rune names to rune codes;
   Names are stored in uppercase.")

(defun define-rune-name (name code)
  (setf (gethash (string-upcase name) *rune-names*) code)
  name)

(defun lookup-rune-name (name)
  (gethash (string-upcase name) *rune-names*))

(define-rune-name "null"        #x0000)
(define-rune-name "space"       #x0020)
(define-rune-name "newline"     #x000A)
(define-rune-name "return"      #x000D)
(define-rune-name "tab"         #x0009)
(define-rune-name "page"        #x000C)

;; and just for fun:
(define-rune-name "euro"        #x20AC)

;; ASCII control characters
(define-rune-name "nul"  #x0000)        ;null
(define-rune-name "soh"  #x0001)        ;start of header
(define-rune-name "stx"  #x0002)        ;start of text
(define-rune-name "etx"  #x0003)        ;end of text
(define-rune-name "eot"  #x0004)        ;end of transmission
(define-rune-name "enq"  #x0005)        ;
(define-rune-name "ack"  #x0006)        ;acknowledge
(define-rune-name "bel"  #x0007)        ;bell
(define-rune-name "bs"   #x0008)        ;backspace
(define-rune-name "ht"   #x0009)        ;horizontal tab
(define-rune-name "lf"   #X000A)        ;line feed, new line
(define-rune-name "vt"   #X000B)        ;vertical tab
(define-rune-name "ff"   #x000C)        ;form feed
(define-rune-name "cr"   #x000D)        ;carriage return
(define-rune-name "so"   #x000E)        ;shift out
(define-rune-name "si"   #x000F)        ;shift in
(define-rune-name "dle"  #x0010)        ;device latch enable ?
(define-rune-name "dc1"  #x0011)        ;device control 1
(define-rune-name "dc2"  #x0012)        ;device control 2
(define-rune-name "dc3"  #x0013)        ;device control 3
(define-rune-name "dc4"  #x0014)        ;device control 4
(define-rune-name "nak"  #x0015)        ;negative acknowledge
(define-rune-name "syn"  #x0016)        ;
(define-rune-name "etb"  #x0017)        ;
(define-rune-name "can"  #x0018)        ;
(define-rune-name "em"   #x0019)        ;end of message
(define-rune-name "sub"  #x001A)        ;
(define-rune-name "esc"  #x001B)        ;escape
(define-rune-name "fs"   #x001C)        ;field separator ?
(define-rune-name "gs"   #x001D)        ;group separator
(define-rune-name "rs"   #x001E)        ;
(define-rune-name "us"   #x001F)        ;
 
(define-rune-name "del"  #x007F)        ;delete

;; iso-latin
(define-rune-name "nbsp" #x00A0)        ;non breakable space
(define-rune-name "shy"  #x00AD)        ;soft hyphen

(defun rune-from-read-name (name)
  (code-rune
   (cond ((= (length name) 1)
           (iso-10646-char-code (char name 0)))
     ((and (= (length name) 2)
           (char= (char name 0) #\\))
       (iso-10646-char-code (char name 1)))
     ((and (>= (length name) 3)
           (char-equal (char name 0) #\u)
           (char-equal (char name 1) #\+)
           (every (lambda (x) (digit-char-p x 16)) (subseq name 2)))
       (parse-integer name :start 2 :radix 16))
     ((lookup-rune-name name))
     (t
       (error "Meaningless rune name ~S." name)))))

(defun rune-reader (stream subchar arg)
  subchar arg
  (values (rune-from-read-name (read-rune-name stream))))

(set-dispatch-macro-character #\# #\/ 'rune-reader)

;;; ROD ext syntax

(defun rod-reader (stream subchar arg)
  (declare (ignore arg))
  (rod
   (with-output-to-string (bag)
     (do ((c (read-char stream t nil t)
             (read-char stream t nil t)))
         ((char= c subchar))
       (cond ((char= c #\\)
              (setf c (read-char stream t nil t))))
       (princ c bag)))))

#-rune-is-character
(defun rod-printer (stream rod)
  (princ #\# stream)
  (princ #\" stream)
  (loop for x across rod do
        (cond ((or (rune= x #.(char-rune #\\))
                   (rune= x #.(char-rune #\")))
               (princ #\\ stream)
               (princ (code-char x) stream))
              ((< x char-code-limit)
               (princ (code-char x) stream))
              (t
               (format stream "\\u~4,'0X" x))))
  (princ #\" stream))

(set-dispatch-macro-character #\# #\" 'rod-reader)
