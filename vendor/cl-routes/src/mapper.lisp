;;;; mapper.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:routes)

(defclass mapper ()
  ((template :initform nil :initarg :template)))

(defun connect (map route)
  (let ((spec (slot-value map 'template))
        (route-spec (concatenate 'list
                                 (route-template route)
                                 (list route))))
    (setf (slot-value map 'template)
          (if spec
              (merge-uri-templates spec route-spec)
              (concatenate 'list
                           route-spec)))))

(defun reset-mapper (map)
  (setf (slot-value map 'template) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric match (map uri &optional bindings))

(defmethod match (map (empty (eql nil)) &optional (bindings +no-bindings+))
  (match map '(nil) bindings))

(defmethod match (map (uri string) &optional (bindings +no-bindings+))
  (match map (quri:uri uri) bindings))

(defmethod match (map (uri quri:uri) &optional (bindings +no-bindings+))
  (match map (or (cdr (split-sequence #\/ (quri:uri-path uri))) '("")) bindings))

(defmethod match (map (route route) &optional (bindings +no-bindings+))
  (match map (route-template route) bindings))

(defmethod match (map (paths cons) &optional (bindings +no-bindings+))
  (let ((res (unify (slot-value map 'template)
                    (if (car paths)
                        (concatenate 'list
                                     (apply-bindings paths bindings)
                                     (list (make-unify-template 'variable
                                                                'routes:route)))
                        (list (make-unify-template 'variable
                                                   'routes:route)))
                    bindings)))
    (if res
        (let ((route (cdar res))
              (bindings (cdr res)))
          (values route
                  (reverse bindings))))))
