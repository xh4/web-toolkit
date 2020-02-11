;;; copyright (c) 2005 David Lichteblau <david@lichteblau.com>
;;; License: Lisp-LGPL (See file COPYING for details).
;;;
;;; Rune emulation for the UTF-8-compatible DOM implementation.
;;; Used only with 8 bit characters on non-unicode Lisps.

(in-package :utf8-runes)

(deftype rune () 'character)
(deftype rod () '(vector rune))
(deftype simple-rod () '(simple-array rune))

(defun rod= (r s)
  (string= r s))

(defun rod-string (rod &optional default)
  (declare (ignore default))
  rod)

(defun string-rod (string)
  string)

(defun make-rod (size)
  (make-string size :element-type 'rune))

(defun rune-reader (stream subchar arg)
  (runes::rune-char (runes::rune-reader stream subchar arg)))

(defun rod-reader (stream subchar arg)
  (runes::rod-string (runes::rod-reader stream subchar arg)))

(setf closure-common-system:*utf8-runes-readtable*
      (let ((rt (copy-readtable)))
	(set-dispatch-macro-character #\# #\/ 'rune-reader rt)
	(set-dispatch-macro-character #\# #\" 'rod-reader rt)
	rt))
