;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)


(defvar *arity-p-of* (make-hash-table :test 'eq))


(defmacro lambda-list-arity-p (lambda-list)
  (loop
     with op = 'eql
     with op-text = "equal to"
     with count = 0
     for token in lambda-list
     until (when (eq token '&rest)
             (setf op '>=
                   op-text "greater than"))
     do (incf count)
     finally (return `(list (function (lambda (n)
                              (,op n ,count)))
                            ,(format nil "~A ~A" op-text count)))))


(defun check-op-arity (op arg-count)
  (let ((found (gethash op *arity-p-of*)))
    (when found
      (destructuring-bind (arity-p text) found
          (unless (funcall arity-p arg-count)
            (error 'invalid-op-vars-error
                   :message (format nil
                                    "Invalid variable count. The number of ~
                                     variables for operator ~A must be ~A."
                                    op text)))))))


(defmacro define-operator (name (arg &rest arguments) &body body)
  `(progn
     (setf (gethash ',name *arity-p-of*)
           (lambda-list-arity-p ,arguments))
     (defmacro ,name ,(cons arg arguments)
       ,@body)))


(define-operator cl-uri-templates.operators:-opt (argument variable)
  `(if ,variable
       ,argument
       ""))


(define-operator cl-uri-templates.operators:-neg (argument variable)
  `(if ,variable
       ""
       ,argument))


(define-operator cl-uri-templates.operators:-prefix (argument variable)
  `(let ((var ,variable))
     (if (consp var)
         (apply #'concatenate 'string (loop
                                         for v in var
                                         collect ,argument
                                         collect (princ-to-string (or v ""))))
         (concatenate 'string ,argument (princ-to-string (or var ""))))))


(define-operator cl-uri-templates.operators:-suffix (argument variable)
  `(let ((var ,variable))
     (if (consp var)
         (apply #'concatenate 'string (loop
                                         for v in ,variable
                                         collect (princ-to-string (or v ""))
                                         collect ,argument))
         (concatenate 'string (princ-to-string (or var "")) ,argument))))


(define-operator cl-uri-templates.operators:-join (argument &rest variables)
  `(macrolet
       ((cl-uri-templates::uri-template-var (var &optional (default ""))
          `(cons ,(string-downcase (symbol-name var))
                 (expand-uri-template-var ,var ,default))))
     (apply #'concatenate 'string (loop
                                     for var in (list ,@variables)
                                     for sep = "" then ,argument
                                     for (name . value) = var
                                     collect sep
                                     collect name
                                     collect "="
                                     collect (princ-to-string
                                              (or value ""))))))

#+nil(-join "/"
            (cl-uri-templates::uri-template-var a)
            (cl-uri-templates::uri-template-var b))

(define-operator cl-uri-templates.operators:-list (argument variable)
  `(let ((var ,variable))
     (assert (typep var 'list)
             () 'cl-uri-templates:invalid-op-vars-error
             "Operator -list only accepts a variable containing a list.")
     (apply #'concatenate 'string (loop
                                     for v in var
                                     for sep = "" then ,argument
                                     collect sep
                                     collect (princ-to-string (or v ""))))))
