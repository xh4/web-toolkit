;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; Encoding: utf-8; -*-

(defpackage :closure-system (:use #:asdf #:cl))
(in-package :closure-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

;;; Convenience feature: will stop it from breaking into the debugger
;;; under sbcl for full WARNINGs (better to fix the warnings :-).
#+sbcl
(defmethod perform :around ((o compile-op) s)
  (setf (operation-on-failure o) :warn)
  (call-next-method o s))

(asdf:defsystem closure-html
    :default-component-class closure-source-file
    :components
    ((:module src
	      :serial t
	      :components 
	      (;; Early package definitions
	       
	       (:file "defpack")

	       ;; glisp

	       (:module glisp
			:pathname "glisp/"
			:components ((:file "util")))

	       ;; CLEX and LALR
	       
	       (:module clex
			:pathname "util/"
			:components
			((:file "clex")))
               
	       (:module lalr
			:pathname "util/"
			:components
			((:file "lalr")))

	       ;; Networking stuff
     
	       (:module net
			:pathname "net/"
			:components ((:file "mime")))

	       ;; The HTML parser
     
	       (:module parse
			:depends-on (clex lalr)
			:components
			((:file "pt")
			 (:file "sgml-dtd")
			 (:file "sgml-parse"
				:depends-on ("sgml-dtd" "pt"))
			 (:file "html-parser"
				:depends-on ("sgml-parse"))
			 (:file "lhtml"
				:depends-on ("html-parser"))
			 (:file "unparse"
				:depends-on ("html-parser"))
			 (:file "documentation"
				:depends-on ("html-parser")))))))
    :depends-on (:closure-common :flexi-streams))
