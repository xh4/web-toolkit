;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)


(defvar *decode-uri-string* t)


;; RFC 2396 standard URI components
(defvar %uri-scheme)
(defvar %uri-authority)
(defvar %uri-path)
(defvar %uri-query)
(defvar %uri-fragment)


;; extended URI components
(defvar %uri-head)
(defvar %uri-tail)
(defvar %uri-user)
(defvar %uri-host)
(defvar %uri-port)
(defvar %uri-directory)
(defvar %uri-file)


(defmacro bind-authority-components (authority &body body)
  (let ((t1 (gensym)) (t2 (gensym)))
    `(destructuring-bind (&optional ,t1 %uri-user %uri-host ,t2 %uri-port)
         (coerce
          (nth-value
           1
           (cl-ppcre:scan-to-strings "(([^@]+)@)?([^\\:]+)(\\:(\\d+))?"
                                     ,authority))
          'list)
       (declare (ignore ,t1 ,t2))
       ,@body)))


(defmacro bind-path-components (path &body body)
  `(destructuring-bind (&optional %uri-directory %uri-file)
       (coerce
        (nth-value
         1
         (cl-ppcre:scan-to-strings "(.*/)([^/]+)?"
                                   ,path))
        'list)
     ,@body))


(defmacro bind-standard-uri-components (uri-reference &body body)
  (let ((t1 (gensym)) (t2 (gensym)) (t3 (gensym)) (t4 (gensym)))
    `(destructuring-bind (&optional %uri-head ,t1 %uri-scheme ,t2 %uri-authority %uri-tail %uri-path ,t3 %uri-query ,t4 %uri-fragment)
         (coerce
          (nth-value
           1
           (cl-ppcre:scan-to-strings
            ;; regex adapted from RFC 2396
            ;; "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
            "^((([^:/?#]+):)?(//([^/?#]*)))?(([^?#]*)(\\?([^#]*))?(#(.*))?)"
            ,uri-reference))
          'list)
       (declare (ignore ,t1 ,t2 ,t3 ,t4))
       (bind-authority-components %uri-authority
         (bind-path-components %uri-path
           ,@body)))))


(defmacro uri-template-bind ((template) uri &body body)
  "Binds URI template placeholders (which must be either symbols or
lists like (#'parse-fn var), much like arguments to
cl-ppcre:register-groups-bind) in given URI to the specified
variables, as well as binding a set of standard special
variables (%uri-protocol, %uri-host, etc.) to their respective parts
of the given URI."
  (let* ((template (cdr template)) ;; template is expected to look like output of #U: '(uri-template &rest args)
         (template-bindings (mapcar #'second (remove-if #'stringp template)))
         (template-vars (loop for x in template-bindings when (symbolp x) collect x))
         (uri-var (gensym)))
    `(let ((,uri-var ,uri))
       (bind-standard-uri-components ,uri-var
         (register-groups-bind ,template-bindings
             ('(:sequence
                :start-anchor
                ,@(substitute-if-not '(:REGISTER (:GREEDY-REPETITION 0 NIL :EVERYTHING))
                                     #'stringp template)
                :end-anchor)
               ,uri-var)
           ,(when *decode-uri-string*
                  `(setf ,@(mapcan (lambda (var)
                                     `(,var (uri-decode ,var)))
                                   template-vars)))
           ,@body)))))


(defstruct uri-scanner function variables)

(defvar *variables*)


(defgeneric uri-compound-form-to-parse-tree (symbol arguments))

(defmethod uri-compound-form-to-parse-tree ((fun-name (eql 'uri-template-var))
                                            (arguments list))
  (declare (ignore fun-name))
  (destructuring-bind (var-name &optional var-default) arguments
    (push (list var-name var-default) *variables*)
    '(:register (:non-greedy-repetition 0 nil :everything))))

(defmethod uri-compound-form-to-parse-tree ((fun-name (eql '-opt))
                                            (arguments list))
  (declare (ignore fun-name))
  (destructuring-bind (arg (var-fun var-name &optional var-default)) arguments
    (assert (eq var-fun 'uri-template-var))
    '(:register (:non-greedy-repetition 0 nil :everything))))


(defgeneric uri-form-to-parse-tree (form))

(defmethod uri-form-to-parse-tree ((form cons))
  (uri-compound-form-to-parse-tree (first form) (rest form)))

(defmethod uri-form-to-parse-tree ((form string))
  form)


(defgeneric uri-template-to-scanner (template))

(defmethod uri-template-to-scanner ((template-form cons))
  (assert (eql (first template-form) 'uri-template))
  (let* (*variables*
         (parse-tree `(:sequence
                       :start-anchor
                       ,@(mapcar #'uri-form-to-parse-tree
                                 (rest template-form))
                       :end-anchor)))
    ;;(format t "Vars: ~S~%Parse tree : ~S~%" *variables* parse-tree)
    (make-uri-scanner :function (cl-ppcre:create-scanner parse-tree)
                      :variables (reverse *variables*))))

(defmethod uri-template-to-scanner ((template string))
  (uri-template-to-scanner (parse-uri-template template)))


(defgeneric destructure-uri (uri template))

(defmethod destructure-uri (uri (template uri-scanner))
  (loop
     for var in (uri-scanner-variables template)
     for var-name = (car var)
     with regs = (nth-value 1 (cl-ppcre:scan-to-strings
                               (uri-scanner-function template)
                               uri))
     for i below (length regs)
     for reg = (aref regs i)
     do (setf (uri-var var-name) reg))
  *uri-environment*)

(defmethod destructure-uri (uri (template string))
  (destructure-uri uri (uri-template-to-scanner template)))

(defmacro with-destructured-uri (uri template variables &body body)
  `(with-uri-environment
     (destructure-uri ,uri ,template)
     (with-uri-variables ,variables
       ,@body)))
