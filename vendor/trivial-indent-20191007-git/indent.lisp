#|
 This file is a part of Trivial-Indent
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trivial-indent
  (:use #:cl)
  (:nicknames #:org.tymoonnext.radiance.lib.trivial-indent #:indent)
  (:export
   #:indentation
   #:define-indentation
   #:remove-indentation
   #:initialize-slime
   #:initialize-sly))

(in-package #:org.tymoonnext.radiance.lib.trivial-indent)

(defvar *indentation-hints* (make-hash-table :test #'eq))

(defun indentation (symbol)
  "Returns the custom defined indentation of a symbol if there is any. SETF-able."
  (gethash symbol *indentation-hints*))

(defun (setf indentation) (rule-form symbol)
  "Sets the indentation hint for a symbol."
  (setf (gethash symbol *indentation-hints*) rule-form))

(defmacro define-indentation (symbol rule-form)
  "Define an indentation hint for a symbol.

See the SLIME/SWANK documentation for more information on the rules.
Example: (define-indentation defmacro (4 &lambda &body))"
  (assert (symbolp symbol))
  (assert (listp rule-form))
  `(setf (indentation ',symbol) ',rule-form))

(defun remove-indentation (symbol)
  "Remove the indentation hint for a symbol."
  (remhash symbol *indentation-hints*))

(defun initialize-slime ()
  "Attempts to initialize slime with our indentation table.
If SWANK-INDENTATION is not loaded, this does nothing.
It should be safe to call this function regardless of whether
SWANK is loaded at all or not.

This is automatically called when TRIVIAL-INDENT is loaded."
  (when (member "SWANK-INDENTATION" *modules* :test #'string=)
    (let* ((swank (find-package :swank))
           (tables (when swank (find-symbol (string '#:*application-hints-tables*) swank))))
      (when tables
        (set tables (cons *indentation-hints* (remove *indentation-hints* (symbol-value tables))))
        t))))

(defun initialize-sly ()
  "Attempts to initialize sly with our indentation table.
If SLYNK/INDENTATION is not loaded, this does nothing.
It should be safe to call this function regardless of whether
SLYNK is loaded at all or not.

This is automatically called when TRIVIAL-INDENT is loaded."
  (when (member "SLYNK/INDENTATION" *modules* :test #'string=)
    (let* ((slynk (find-package :slynk))
           (tables (when slynk (find-symbol (string '#:*application-hints-tables*) slynk))))
      (when tables
        (set tables (cons *indentation-hints* (remove *indentation-hints* (symbol-value tables))))
        t))))

(initialize-slime)
(initialize-sly)
