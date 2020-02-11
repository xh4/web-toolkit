;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: definline
;;;   Created: 1999-05-25 22:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: Lisp-LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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

#-(or allegro openmcl)
(defmacro definline (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))

#+openmcl
(defmacro runes::definline (fun args &body body)
  (if (consp fun)
      `(defun ,fun ,args
         ,@body)
      `(progn
         (defun ,fun ,args .,body)
         (define-compiler-macro ,fun (&rest .args.)
           (cons '(lambda ,args .,body)
                 .args.)))))

#+allegro
(defmacro definline (fun args &body body)
  (if (and (consp fun) (eq (car fun) 'setf))
      (let ((fnam (intern (concatenate 'string "(SETF " (symbol-name (cadr fun)) ")")
                          (symbol-package (cadr fun)))))
        `(progn
           (defsetf ,(cadr fun) (&rest ap) (new-value) (list* ',fnam new-value ap))
           (definline ,fnam ,args .,body)))
    (labels ((declp (x)
               (and (consp x) (eq (car x) 'declare))))
      `(progn
         (defun ,fun ,args .,body)
         (define-compiler-macro ,fun (&rest .args.)
           (cons '(lambda ,args
                   ,@(remove-if-not #'declp body)
                   (block ,fun 
                     ,@(remove-if #'declp body)))
                 .args.))))))
