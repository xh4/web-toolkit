;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)

(defvar *uri-environment*)

(defmacro with-uri-environment (&body body)
  `(let (*uri-environment*)
     ,@body))

(defun uri-var (name)
  (ignore-errors
    (getf *uri-environment* name)))

(defsetf uri-var (name) (value)
  `(setf (getf *uri-environment* ,name) ,value))

(defmacro with-uri-variables (variables &body body)
  `(symbol-macrolet ,(loop
                        for var in variables
                        collect `(,var (uri-var ',var)))
     ,@body))

(defmacro uri-variables-setq (&rest variables)
  "This macro sets values of bound variables from uri environment"
  (loop
     for var in variables
     collect `(setq ,var (uri-var ',var))))
