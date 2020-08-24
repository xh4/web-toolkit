;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)


(defmacro expand-uri-template-var (var &optional default)
  `(or (uri-var ',var)
       (handler-case ,var
         (unbound-variable ()
           ,default))))


(defmacro expand-uri-template (template)
  (declare (type uri-template template))
  `(macrolet ((uri-template-var (var &optional default)
                `(expand-uri-template-var ,var ,default)))
     ,(if (stringp template)
                   (parse-uri-template template)
                   template)))


(defun uri-template (&rest template-args)
  (format nil "~{~@[~A~]~}" template-args))


#+parenscript (parenscript:defpsmacro uri-template (&rest template-args)
                `(+ ,@template-args))
