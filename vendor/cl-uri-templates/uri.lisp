;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)


(defvar *characters* (make-array '(256 2)
                                 :element-type 'boolean
                                 :initial-element nil))


(defconstant +uri-reserved-char-p-column+   0)
(defconstant +uri-unreserved-char-p-column+ 1)


(defmacro uri-reserved-char-p (char)
  `(aref *characters* (char-code ,char) +uri-reserved-char-p-column+))


(defmacro uri-unreserved-char-p (char)
  `(aref *characters* (char-code ,char) +uri-unreserved-char-p-column+))


(map nil #'(lambda (c)
             (setf (uri-reserved-char-p c) t))
     ";/?:@&=+$,")


(map nil #'(lambda (c)
             (setf (uri-unreserved-char-p c) t))
     "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.!~*'()")


(defun uri-encode (string)
  (declare (type string string))
  (coerce (loop
             with bytes = (trivial-utf-8:string-to-utf-8-bytes string)
             for i of-type fixnum from 0 below (length bytes)
             for b of-type (unsigned-byte 8) = (aref bytes i)
             if (uri-unreserved-char-p (code-char b))
             collect (code-char b)
             else collect #\%
             and  collect (digit-char (floor b 16) 16)
             and  collect (digit-char (mod b 16) 16))
          'string))


#+parenscript (parenscript:defpsmacro uri-encode (x)
                `(encode-u-r-i-component ,x))


(defun uri-decode (string)
  (declare (type string string))
  (trivial-utf-8:utf-8-bytes-to-string
   (coerce (loop
              with bytes = (trivial-utf-8:string-to-utf-8-bytes string)
              for i of-type fixnum from 0 below (length bytes)
              for b of-type (unsigned-byte 8) = (aref bytes i)
              if (eql (char-code #\%) b)
              do (assert (< (+ i 2)
                            (length string))
                         nil "Incomplete percent-encoding in URI ~S." string)
              and do (incf i)
              and collect (parse-integer string
                                         :start i
                                         :end (+ i 2)
                                         :radix 16)
              and do (incf i)
              else collect b)
           '(vector (unsigned-byte 8)))))
