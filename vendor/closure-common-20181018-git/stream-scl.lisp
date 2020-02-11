;;; -*- Mode: Lisp; Syntax: Common-Lisp; readtable: runes; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Fast streams
;;;   Created: 1999-07-17
;;;    Author: Douglas Crosher
;;;   License: Lisp-LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2007 by Douglas Crosher

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

(in-package :runes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fast* '(optimize (speed 3) (safety 3))))

(deftype runes-encoding:encoding-error ()
  'ext:character-conversion-error)


;;; xstream

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass xstream (ext:character-stream)
  ((name :initarg :name :initform nil
	 :accessor xstream-name)
   (column :initarg :column :initform 0)
   (line :initarg :line :initform 1)
   (unread-column :initarg :unread-column :initform 0)))

(defclass eol-conversion-xstream (lisp::eol-conversion-input-stream xstream)
  ())

) ; eval-when

(defun make-eol-conversion-xstream (source-stream)
  "Returns a character stream that conversion CR-LF pairs and lone CR
  characters into single linefeed character."
  (declare (type stream source-stream))
  (let ((stream (ext:make-eol-conversion-stream source-stream
						:input t
						:close-stream-p t)))
    (change-class stream 'eol-conversion-xstream)))

(definline xstream-p (stream)
  (typep stream 'xstream))

(defun close-xstream (input)
  (close input))

(definline read-rune (input)
  (declare (type stream input)
	   (inline read-char)
	   #.*fast*)
  (let ((char (read-char input nil :eof)))
    (cond ((member char '(#\UFFFE #\UFFFF))
	   ;; These characters are illegal within XML documents.
	   (simple-error 'ext:character-conversion-error
			 "~@<Illegal XML document character: ~S~:@>" char))
	  ((eql char #\linefeed)
	   (setf (slot-value input 'unread-column) (slot-value input 'column))
	   (setf (slot-value input 'column) 0)
	   (incf (the kernel:index (slot-value input 'line))))
	  (t
	   (incf (the kernel:index (slot-value input 'column)))))
    char))

(definline peek-rune (input)
  (declare (type stream input)
	   (inline peek-char)
	   #.*fast*)
  (peek-char nil input nil :eof))

(definline consume-rune (input)
  (declare (type stream input)
	   (inline read-rune)
	   #.*fast*)
  (read-rune input)
  nil)

(definline unread-rune (rune input)
  (declare (type stream input)
	   (inline unread-char)
	   #.*fast*)
  (unread-char rune input)
  (cond ((eql rune #\linefeed)
	 (setf (slot-value input 'column) (slot-value input 'unread-column))
	 (setf (slot-value input 'unread-column) 0)
	 (decf (the kernel:index (slot-value input 'line))))
	(t
	 (decf (the kernel:index (slot-value input 'column)))))
  nil)

(defun fread-rune (input)
  (read-rune input))

(defun fpeek-rune (input)
  (peek-rune input))

(defun xstream-position (input)
  (file-position input))

(defun runes-encoding:find-encoding (encoding)
  encoding)

(defun make-xstream (os-stream &key name
                                    (speed 8192)
                                    (initial-speed 1)
                                    (initial-encoding :guess))
  (declare (ignore speed))
  (assert (eql initial-speed 1))
  (assert (eq initial-encoding :guess))
  (let* ((stream (ext:make-xml-character-conversion-stream os-stream
							   :input t
							   :close-stream-p t))
	 (xstream (make-eol-conversion-xstream stream)))
    (setf (xstream-name xstream) name)
    xstream))


(defclass xstream-string-input-stream (lisp::string-input-stream xstream)
  ())

(defun make-rod-xstream (string &key name)
  (declare (type string string))
  (let ((stream (make-string-input-stream string)))
    (change-class stream 'xstream-string-input-stream :name name)))

;;; already at 'full speed' so just return the buffer size.
(defun set-to-full-speed (stream)
  (length (ext:stream-in-buffer stream)))

(defun xstream-speed (stream)
  (length (ext:stream-in-buffer stream)))

(defun xstream-line-number (stream)
  (slot-value stream 'line))

(defun xstream-column-number (stream)
  (slot-value stream 'column))

(defun xstream-encoding (stream)
  (stream-external-format stream))

;;; the encoding will have already been detected, but it is checked against the
;;; declared encoding here.
(defun (setf xstream-encoding) (declared-encoding stream)
  (let* ((initial-encoding (xstream-encoding stream))
	 (canonical-encoding
	  (cond ((and (eq initial-encoding :utf-16le)
		      (member declared-encoding '(:utf-16 :utf16 :utf-16le :utf16le)
			      :test 'string-equal))
		 :utf-16le)
		((and (eq initial-encoding :utf-16be)
		      (member declared-encoding '(:utf-16 :utf16 :utf-16be :utf16be)
			      :test 'string-equal))
		 :utf-16be)
		((and (eq initial-encoding :ucs-4be)
		      (member declared-encoding '(:ucs-4 :ucs4 :ucs-4be :ucs4be)
			      :test 'string-equal))
		 :ucs4-be)
		((and (eq initial-encoding :ucs-4le)
		      (member declared-encoding '(:ucs-4 :ucs4 :ucs-4le :ucs4le)
			      :test 'string-equal))
		 :ucs4-le)
		(t
		 declared-encoding))))
    (unless (string-equal initial-encoding canonical-encoding)
      (warn "Unable to change xstream encoding from ~S to ~S (~S)~%"
	    initial-encoding declared-encoding canonical-encoding))
    declared-encoding))


;;; ystream - a run output stream.

(deftype ystream () 'stream)

(defun ystream-column (stream)
  (ext:line-column stream))

(definline write-rune (rune stream)
  (declare (inline write-char))
  (write-char rune stream))

(defun write-rod (rod stream)
  (declare (type rod rod)
	   (type stream stream))
  (write-string rod stream))

(defun make-rod-ystream ()
  (make-string-output-stream))

(defun close-ystream (stream)
  (etypecase stream
    (ext:string-output-stream
     (get-output-stream-string stream))
    (ext:character-conversion-output-stream
     (let ((target (slot-value stream 'stream)))
       (close stream)
       (if (typep target 'ext:byte-output-stream)
	   (ext:get-output-stream-bytes target)
	   stream)))))

;;;; CHARACTER-STREAM-YSTREAM

(defun make-character-stream-ystream (target-stream)
  target-stream)


;;;; OCTET-VECTOR-YSTREAM

(defun make-octet-vector-ystream ()
  (let ((target (ext:make-byte-output-stream)))
    (ext:make-character-conversion-stream target :output t
					  :external-format :utf-8
					  :close-stream-p t)))

;;;; OCTET-STREAM-YSTREAM

(defun make-octet-stream-ystream (os-stream)
  (ext:make-character-conversion-stream os-stream :output t
					:external-format :utf-8
					:close-stream-p t))


;;;; helper functions

(defun rod-to-utf8-string (rod)
  (ext:make-string-from-bytes (ext:make-bytes-from-string rod :utf8)
			      :iso-8859-1))

(defun utf8-string-to-rod (str)
  (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code str)))
    (ext:make-string-from-bytes bytes :utf-8)))

(defun make-octet-input-stream (octets)
  (ext:make-byte-input-stream octets))


