;; xhtmlgen.lisp
;; This version by david@lichteblau.com for headcraft (http://headcraft.de/)
;;
;; Derived from htmlgen.cl:
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA

(defpackage :xhtml-generator
  (:use :common-lisp)
  (:export #:with-html #:write-doctype))

(in-package :xhtml-generator)

;; html generation

(defstruct (html-process (:type list) (:constructor
				       make-html-process (key macro special
							      name-attr
							      )))
  key		; keyword naming this tag
  macro  	; the macro to define this
  special       ; if true then call this to process the keyword and return
                ; the macroexpansion
  name-attr     ; attribute symbols which can name this object for subst purposes
  )


(defparameter *html-process-table* 
    (make-hash-table :test #'equal) ; #'eq is accurate but want to avoid rehashes
  )

(defvar *html-sink*)

(defun write-doctype (sink)
  (sax:start-dtd sink
                 "html"
                 "-//W3C//DTD XHTML 1.0 Transitional//EN"
                 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")
  (sax:end-dtd sink))

(defmacro with-html (sink &rest forms &environment env)
  `(let ((*html-sink* ,sink))
     ,(process-html-forms forms env)))

(defun get-process (form)
  (let ((ent (gethash form *html-process-table*)))
    (unless ent
      (error "unknown html keyword ~s" form))
    ent))

(defun process-html-forms (forms env)
  (let (res)
    (flet ((do-ent (ent args argsp body)
             ;; ent is an html-process object associated with the 
             ;;	    html tag we're processing
             ;; args is the list of values after the tag in the form
             ;;     ((:tag &rest args) ....)
             ;; argsp is true if this isn't a singleton tag  (i.e. it has
             ;;     a body) .. (:tag ...) or ((:tag ...) ...)
             ;; body is the body if any of the form
             ;; 
	     (let ((special (html-process-special ent)))
	       (push (if special
                         (funcall special ent args argsp body)
                         `(,(html-process-macro ent)
                           ,args
                           ,(process-html-forms body env)))
                     res))))
      (do* ((xforms forms (cdr xforms))
	    (form (car xforms) (car xforms)))
	  ((null xforms))

	(setq form (macroexpand form env))
	
	(if (atom form)
            (typecase form
              (keyword (do-ent (get-process form) nil nil nil)) 
              (string (push `(sax:characters *html-sink* ,form) res))
              (t (push form res)))
            (let ((first (car form)))
              (cond
                ((keywordp first)
                  ;; (:xxx . body) form
                  (do-ent (get-process (car form)) nil t (cdr form)))
                 ((and (consp first) (keywordp (car first)))
                   ;; ((:xxx args ) . body)
                   (do-ent (get-process (caar form)) (cdr first) t (cdr form)))
                (t
                  (push form res)))))))
    `(progn ,@(nreverse res))))

(defun html-body-key-form (string-code args body)
  (unless (evenp (length args))
    (error "attribute list ~S isn't even" args))
  `(let ((.tagname. ,string-code))
     (sax:start-element *html-sink* nil nil .tagname.
                        (list
                         ,@(loop
                               for (name value) on args by #'cddr
                               collect
                                 `(sax:make-attribute
                                   :qname ,(etypecase name
                                             (symbol (symbol-name name))
                                             (string name))
                                   :value ,value
                                   :specified-p t))))
     ,@body
     (sax:end-element *html-sink* nil nil .tagname.)))

(defun emit-without-quoting (str)
  (let ((s (cxml::chained-handler *html-sink*)))
    (cxml::maybe-close-tag s)
    (map nil (lambda (c) (cxml::write-rune (char-code c) s)) str)))

(defun princ-http (val)
  (warn "use of deprecated :PRINC (use :PRINC-SAFE instead?)")
  (emit-without-quoting (princ-to-string val)))

(defun prin1-http (val)
  (warn "use of deprecated :PRIN1 (use :PRIN1-SAFE instead?)")
  (emit-without-quoting (prin1-to-string val)))

(defun princ-safe-http (val)
  (sax:characters *html-sink* (princ-to-string val)))

(defun prin1-safe-http (val)
  (sax:characters *html-sink* (prin1-to-string val)))


;; --  defining how html tags are handled. --
;;
;; most tags are handled in a standard way and the def-std-html
;; macro is used to define such tags
;;
;; Some tags need special treatment and def-special-html defines
;; how these are handled.  The tags requiring special treatment
;; are the pseudo tags we added to control operations
;; in the html generator.
;; 
;;
;; tags can be found in three ways:
;;  :br	    		- singleton, no attributes, no body
;;  (:b "foo")          - no attributes but with a body
;;  ((:a href="foo") "balh")  - attributes and body
;;
  
(defmacro def-special-html (kwd fcn)
  ;; kwd - the tag we're defining behavior for.
  ;; fcn - function to compute the macroexpansion of a use of this
  ;;       tag. args to fcn are: 
  ;;		ent - html-process object holding info on this tag
  ;;		args - list of attribute-values following tag
  ;;		argsp - true if there is a body in this use of the tag
  ;;		body - list of body forms.
  `(setf (gethash ,kwd *html-process-table*) 
     (make-html-process ,kwd nil ,fcn nil)))

(def-special-html :newline 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	(when body
          (error "can't have a body with :newline -- body is ~s" body))
	(emit-without-quoting (string #\newline))))
			       
(def-special-html :princ 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(princ-http ,bod))
			  body))))

(def-special-html :princ-safe 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(princ-safe-http ,bod))
			  body))))

(def-special-html :prin1 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(prin1-http ,bod))
			  body))))

(def-special-html :prin1-safe 
    #'(lambda (ent args argsp body)
	(declare (ignore ent args argsp))
	`(progn ,@(mapcar #'(lambda (bod)
			      `(prin1-safe-http ,bod))
			  body))))

(def-special-html :comment
  #'(lambda (ent args argsp body)
      (declare (ignore ent args argsp body))
      `(warn ":COMMENT in html macro not supported yet")))

(defmacro def-std-html (kwd name-attrs)
  (let ((mac-name (intern (format nil "~a-~a" :with-html kwd)))
	(string-code (string-downcase (string kwd))))
    `(progn (setf (gethash ,kwd *html-process-table*)
	      (make-html-process ,kwd
				     ',mac-name
				     nil
				     ',name-attrs))
	    (defmacro ,mac-name (args &rest body)
	      (html-body-key-form ,string-code args body)))))

(def-std-html :a         nil)
(def-std-html :abbr      nil)
(def-std-html :acronym   nil)
(def-std-html :address   nil)
(def-std-html :applet    nil)
(def-std-html :area      nil)

(def-std-html :b         nil)
(def-std-html :base      nil)
(def-std-html :basefont  nil)
(def-std-html :bdo       nil)
(def-std-html :bgsound   nil)
(def-std-html :big       nil)
(def-std-html :blink     nil)
(def-std-html :blockquote nil)
(def-std-html :body      nil)
(def-std-html :br        nil)
(def-std-html :button    nil)

(def-std-html :caption   nil)
(def-std-html :center    nil)
(def-std-html :cite      nil)
(def-std-html :code      nil)
(def-std-html :col       nil)
(def-std-html :colgroup  nil)

(def-std-html :dd         nil)
(def-std-html :del        nil)
(def-std-html :dfn        nil)
(def-std-html :dir        nil)
(def-std-html :div        nil)
(def-std-html :dl         nil)
(def-std-html :dt         nil)

(def-std-html :em         nil)
(def-std-html :embed      nil)

(def-std-html :fieldset     nil)
(def-std-html :font         nil)
(def-std-html :form         :name)
(def-std-html :frame        nil)
(def-std-html :frameset     nil)

(def-std-html :h1         nil)
(def-std-html :h2         nil)
(def-std-html :h3         nil)
(def-std-html :h4         nil)
(def-std-html :h5         nil)
(def-std-html :h6         nil)
(def-std-html :head       nil)
(def-std-html :hr         nil)
(def-std-html :html       nil)

(def-std-html :i        nil)
(def-std-html :iframe   nil)
(def-std-html :ilayer   nil)
(def-std-html :img      :id)
(def-std-html :input    nil)
(def-std-html :ins      nil)
(def-std-html :isindex  nil)

(def-std-html :kbd  	 nil)
(def-std-html :keygen  	 nil)

(def-std-html :label  	 nil)
(def-std-html :layer  	 nil)
(def-std-html :legend  	 nil)
(def-std-html :li  	 nil)
(def-std-html :link  	 nil)
(def-std-html :listing   nil)

(def-std-html :map  	 nil)
(def-std-html :marquee   nil)
(def-std-html :menu  	 nil)
(def-std-html :meta  	 nil)
(def-std-html :multicol  nil)

(def-std-html :nobr  	 nil)
(def-std-html :noembed   nil)
(def-std-html :noframes  nil)
(def-std-html :noscript  nil)

(def-std-html :object  	 nil)
(def-std-html :ol  	 nil)
(def-std-html :optgroup  nil)
(def-std-html :option  	 nil)

(def-std-html :p  	 nil)
(def-std-html :param  	 nil)
(def-std-html :plaintext nil)
(def-std-html :pre  	 nil)

(def-std-html :q  	 nil)

(def-std-html :s  	 nil)
(def-std-html :samp  	 nil)
(def-std-html :script  	 nil)
(def-std-html :select  	 nil)
(def-std-html :server  	 nil)
(def-std-html :small  	 nil)
(def-std-html :spacer  	 nil)
(def-std-html :span  	 :id)
(def-std-html :strike  	 nil)
(def-std-html :strong  	 nil)
(def-std-html :style     nil)  
(def-std-html :sub  	 nil)
(def-std-html :sup  	 nil)

(def-std-html :table  	 :name)
(def-std-html :tbody  	 nil)
(def-std-html :td  	 nil)
(def-std-html :textarea  nil)
(def-std-html :tfoot  	 nil)
(def-std-html :th  	 nil)
(def-std-html :thead  	 nil)
(def-std-html :title  	 nil)
(def-std-html :tr  	 nil)
(def-std-html :tt  	 nil)

(def-std-html :u 	 nil)
(def-std-html :ul 	 nil)

(def-std-html :var 	 nil)

(def-std-html :wbr  	 nil)

(def-std-html :xmp 	 nil)
