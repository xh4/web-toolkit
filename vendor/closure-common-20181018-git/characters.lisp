;;; copyright (c) 2004 knowledgeTools Int. GmbH
;;; Author of this version: David Lichteblau <david@knowledgetools.de>
;;;
;;; derived from runes.lisp, (c) copyright 1998,1999 by Gilbert Baumann
;;;
;;; License: Lisp-LGPL (See file COPYING for details).
;;;
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

(in-package :runes)

(deftype rune () #-lispworks 'character #+lispworks 'lw:simple-char)
(deftype rod () '(vector rune))
(deftype simple-rod () '(simple-array rune (*)))

(definline rune (rod index)
  (char rod index))

(defun (setf rune) (new rod index)
  (setf (char rod index) new))

(definline %rune (rod index)
  (aref (the simple-string rod) (the fixnum index)))

(definline (setf %rune) (new rod index)
  (setf (aref (the simple-string rod) (the fixnum index)) new))

(defun rod-capitalize (rod)
  (string-upcase rod))

(definline code-rune (x) (code-char x))
(definline rune-code (x) (char-code x))

(definline rune= (x y) 
  (char= x y))

(defun rune-downcase (rune)
  (char-downcase rune))

(definline rune-upcase (rune)
  (char-upcase rune))

(defun rune-upper-case-letter-p (rune)
  (upper-case-p rune))

(defun rune-lower-case-letter-p (rune)
  (lower-case-p rune))

(defun rune-equal (x y)
  (char-equal x y))

(defun rod-downcase (rod)
  (string-downcase rod))

(defun rod-upcase (rod)
  (string-upcase rod))

(definline white-space-rune-p (char)
  (or (char= char #\tab)
      (char= char #.(code-char 10))     ;Linefeed
      (char= char #.(code-char 13))     ;Carriage Return
      (char= char #\space)))

(definline digit-rune-p (char &optional (radix 10))
  (digit-char-p char radix))

(defun rod (x)
  (cond
    ((stringp x)    x)
    ((symbolp x)    (string x))
    ((characterp x) (string x))
    ((vectorp x)    (coerce x 'string))
    ((integerp x)   (string (code-char x)))
    (t              (error "Cannot convert ~S to a ~S" x 'rod))))

(defun runep (x)
  (characterp x))

(defun sloopy-rod-p (x)
  (stringp x))

(defun rod= (x y)
  (if (zerop (length x))
      (zerop (length y))
      (and (plusp (length y)) (string= x y))))

(defun rod-equal (x y)
  (string-equal x y))

(definline make-rod (size)
  (make-string size :element-type 'rune))

(defun char-rune (char)
  char)

(defun rune-char (rune &optional default)
  (declare (ignore default))
  rune)

(defun rod-string (rod &optional (default-char #\?))
  (declare (ignore default-char))
  rod)

(defun string-rod (string)
  string)

;;;;

(defun rune<= (rune &rest more-runes)
  (loop
      for (a b) on (cons rune more-runes)
      while b
      always (char<= a b)))

(defun rune>= (rune &rest more-runes)
  (loop
      for (a b) on (cons rune more-runes)
      while b
      always (char>= a b)))

(defun rodp (object)
  (stringp object))

(defun rod-subseq (source start &optional (end (length source)))
  (unless (stringp source)
    (error "~S is not of type ~S." source 'rod))
  (subseq source start end))

(defun rod-subseq* (source start &optional (end (length source)))
  (rod-subseq source start end))

(defun rod< (rod1 rod2)
  (string< rod1 rod2))
