;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SGML; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A very first approach to parse DTD's directly
;;;   Created: 1997-10-14
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2001 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :SGML)

(defstruct element
  name                          ;the name (a SYMBOL)
  include                       ;set of included elements (a list)
  exclude                       ;set of excluded elements (a list)
  obegin?                       ;optional start tag? (boolean)
  oend?                         ;optional end tag? (boolean)
  attlist)

(defstruct (dtd (:print-function print-dtd))
  name
  elements                      ;EQ-hashtable mapping from SYMBOLs to ELEMENTs
  entities                      ;a simple alist mapping from entity names to entity values
  resolve-info                  ;a EQUAL-hashtable
  elm-surclusion-cache
  root-elements)

(defun print-dtd (self sink depth)
  (declare (ignore depth))
  (cond (*print-readably*
	 (princ "#S" sink)
	 (prin1 (list 'dtd 
		      :name (dtd-name self)
		      :elements (dtd-elements self))
		sink))
	(t
	 (format sink "#<~S ~S>" (type-of self) (dtd-name self)))))

;;;; ------------------------------------------------------------------------------------------
;;;;  Lexical analysis
;;;;

(defvar *entities*)

#||
(defun find-entity (name)
  (cdr (assoc name *entities* :test #'string=)))

(defun add-entity (name value)
  (setf *entities* (nconc *entities* (list (cons name value)))))

(defun new-entities ()
  (setf *entities* nil))
||#

(defun find-entity (name)
  (gethash name *entities*))

(defun add-entity (name value)
  (unless (gethash name *entities*)
    (setf (gethash name *entities*) value)))

(defun new-entities ()
  (setf *entities* (make-hash-table :test #'equal)))

(clex:deflexer dtd
    ((alpha (or (range "a" "z") (range "A" "Z")))
     (digit (range "0" "9"))
     (white (or #\space #\newline #\tab #\page))
     (name-start alpha)
     (name-char (or alpha digit "." "-" "_"))
     
     (string-char (or white (range #\space #\!) (range #\# #\&) (range #\( #\~)))
     
     (name (and name-start (* name-char)))
     (any (or white (range #\space #\~))) )
  
  ;;slurp away white spaces
  ((white))
  ;; slurp away comments
  ("--" (clex:begin 'comment))
  ((clex::in comment "--") (clex:begin 'clex:initial))
  ((clex::in comment any))
  
  (("%" name ";")
   ;;this is a defined entity
   (let ((looked (find-entity (subseq clex:bag 1 (- (length clex:bag) 1)))))
     (cond (looked
            (clex:backup looked)
            #+(OR)
	    (do ((i (- (length looked) 1) (- i 1)))
		((< i 0))
	      (clex:backup (char looked i))))
	   ((error "Entity ~A is not defined." clex:bag)) )))
  (("%" name)
   ;;this is a defined entity
   (let ((looked (find-entity (subseq clex:bag 1))))
     (cond (looked
	    (do ((i (- (length looked) 1) (- i 1)))
		((< i 0))
	      (clex:backup (char looked i))))
	   ((error "Entity ~A is not defined." clex:bag)) )))
  
  ("<!" 
   (return :open))
  (">"
   (return :close))
  ("[" (return #\[))
  ("]" (return #\]))
  ("ENTITY"  (return :entity))
  ("ATTLIST" (return :attlist))
  ("ELEMENT" (return :element))
  ("SYSTEM"   (return :system))
  ("PUBLIC"   (return :public))
  ("CDATA"    (return :cdata))
  ("IGNORE"    (return :ignore))
  ("INCLUDE"    (return :include))
  ("#REQUIRED" (return :required))
  ("#IMPLIED" (return :implied))
  ("#PCDATA"  (return :pcdata))
  ("#FIXED"  (return :fixed))
  (name (return (list :name clex:bag)))

  ("+(" (clex:backup #\() (return :plus-prefix))
  ("-(" (clex:backup #\() (return :minus-prefix))
  
  ((+ digit)
   (return (list :number (parse-integer clex:bag))))
  
  ;;singetons
  ((or "%()|+-*?,&")
   (return (char clex:bag 0)))
  ;;
  ((and #\" (* (or string-char #\')) #\")
   (return (list :string (subseq clex:bag 1 (- (length clex:bag) 1)))))
  ((and #\' (* (or string-char #\")) #\')
   (return (list :string (subseq clex:bag 1 (- (length clex:bag) 1))))) )

;;;; ------------------------------------------------------------------------------------------
;;;;  Grammar
;;;;

(defvar *dtd*)

(defmacro action (&rest body)
  (let ((args (gensym)))
    `#'(lambda (&rest ,args)
	 (declare (ignorable ,args))
	 (symbol-macrolet (($1 (nth 0 ,args))
			   ($2 (nth 1 ,args))
			   ($3 (nth 2 ,args))
			   ($4 (nth 3 ,args))
			   ($5 (nth 4 ,args))
			   ($6 (nth 5 ,args))
			   ($7 (nth 6 ,args)))
	   ,@body))) )

(lalr:define-grammar dtd-parser (:open :close :entity :attlist :element :name 
				 #\% #\( #\) #\| #\+ #\- #\* #\? #\, #\&
				 :string
				 :plus-prefix :minus-prefix
				 :fixed
				 :required :implied :number :pcdata :cdata
				 :public :system :ignore :include #\[ #\])
  (start --> <definitions> (action nil))

  (<definitions> --> <one-def> :close <definitions> (action nil))
  (<definitions> --> #'(lambda () nil))
  
  (<one-def> --> definition (action (cond ((eq (car $1) 'def-my-entity)
                                           (add-entity (second $1) (third $1)))
					  (t
					   (process-dtd-def *dtd* $1)))))

  (<ignored-definitions> --> ignored-definition :close <ignored-definitions> (action nil))
  (<ignored-definitions> --> (action nil))
  
  (ignored-definition --> definition (action nil))

  (definition --> :open (action nil))
  (definition --> :open :ENTITY #\% :name :string		(action (list 'DEF-MY-ENTITY $4 $5)))
  (definition --> :open :ENTITY #\% :name <sgml-resource>	(action (list 'DEF-MY-ENTITY $4 (sgml-resource-as-string $5))))
  (definition --> :open :ENTITY :name <sgml-resource>		(action (list 'DEF-ENTITY $3 (sgml-resource-as-string $4))))
  (definition --> :open :ATTLIST production attliste		(action (list 'DEFATTLIST $3 $4)))
  (definition --> :open :ELEMENT production odef odef production maybe-pm	(action (list 'DEFELEMENT $3 $4 $5 $6 $7)))

  (definition --> :open #\[ :ignore #\[ <ignored-definitions> #\] #\]
	      #'(lambda (&rest i) i nil))
  (definition --> :open #\[ :include #\[ <definitions> #\] #\]
	      #'(lambda (&rest i) i nil))
  
  (attliste --> #'(lambda () nil))
  (attliste --> att-def attliste		#'cons)
  
  (att-def --> ident production value		#'list)
  (att-def --> ident production :fixed value	#'(lambda (n p i v) i (list n p v)))
  
  (maybe-pm --> #'(lambda () nil))
  (maybe-pm --> :plus-prefix production		#'(lambda (i p) i (list '+ p)))
  (maybe-pm --> :minus-prefix production	#'(lambda (i p) i (list '- p)))
  
  (odef --> #\-   #'(lambda (i) i '-))
  (odef --> ident #'identity)
  (production --> p1 #'identity)
  
  (p1 --> p2 #\| p1				#'(lambda (a b c) b (as-cons 'or a c)))
  (p1 --> p2					#'identity)
  (p2 --> p3 #\, p2				#'(lambda (a b c) b (as-cons 'and a c)))
  (p2 --> p3 #\& p2				#'(lambda (a b c) b (as-cons 'amp a c)))
  (p2 --> p3					#'identity)
  (p3 --> p4 #\*				#'(lambda (a b) b (list '* a)))
  (p3 --> p4 #\+				#'(lambda (a b) b (list '+ a)))
  (p3 --> p4 #\?				#'(lambda (a b) b (list '? a)))
  (p3 --> p4					#'identity)
  (p4 --> #\( production #\)			#'(lambda (a b c) a c b))
  (p4 --> ident					#'identity)
  (p4 --> :pcdata				#'identity)
  (p4 --> :cdata				#'identity)
  (p4 --> :number				#'(lambda (x) (princ-to-string x)))

  (value --> :implied				#'(lambda (a) a :implied))
  (value --> :required				#'(lambda (a) a :required))
  (value --> ident				#'identity)
  (value --> :string				#'identity)
  (value --> :number				#'identity)
  
  (ident --> :name
	 #'(lambda (x) (intern (string-upcase x) :keyword)))

  ( <sgml-resource> --> :cdata  :string		#'list)
  ( <sgml-resource> --> :public :string		#'list)
  ( <sgml-resource> --> :public :string	:string	#'list)
  )

(defun parse-dtd (res-name &optional (top-elment :html))
  (let ((dtd (make-dtd :name res-name :elements (make-hash-table :test #'eq))))
    (with-open-stream (input (apply #'open-sgml-resource res-name))
      (let ((*dtd* dtd)
	    (*entities*)
	    (lexer (make-dtd-lexer input)))
        (new-entities)
	(labels ((next-input ()
		   (let ((x (funcall lexer)))
		     (cond ((eq x :eof) (values :eof :eof))
			   ((atom x) (values x x))
			   (t (values (first x) (second x))))))
		 (parse-error ()
		   ;;(untrack)
		   (error "Parse-Error! at pos = ~D" (ignore-errors (file-position input)))))
	  (dtd-parser #'next-input #'parse-error))))
    (setf (gethash :pcdata (dtd-elements dtd))
      (make-element :name :pcdata
		    :include nil
		    :exclude nil))
    (calculate-resolve-info dtd top-elment)
    dtd))
  
;;;; ------------------------------------------------------------------------------------------

(defun as-cons (op x y)
  (cond ((and (consp y) (eq (car y) op))
	 (list* op x (cdr y)))
	((list op x y))))

(defun find-element (dtd name &optional (intern? nil) (error? t))
  (or (gethash name (dtd-elements dtd))
      (and intern?
	   (let ((new (make-element :name name)))
	     (setf (gethash name (dtd-elements dtd)) new)
	     new))
      (and error? (error "Element ~S is not defined." name))))

(defun find-element-attlist (dtd name)
  (let ((x (find-element dtd name nil nil)))
    (and x (element-attlist x))))

(defun canon-optional-tag-definition (x)
  (cond ((eq x '-) nil)
	((eq x :O) t)
	(t
	 (error "Optionalilty definition must be either '-' or 'O' - ~S. " x))))

(defun production->name-list (prod)
  (cond ((atom prod) (list prod))
	((eq (car prod) 'or)
	 (mapcan #'production->name-list (cdr prod)))
	(t
	 (error "Bogus production - ~S" prod))))

(defun production->name-list/2 (prod)
  (cond ((atom prod) (list prod))
	((member (car prod) '(or and amp + * ?))
	 (mapcan #'production->name-list/2 (cdr prod)))
	(t
	 (error "Bogus production - ~S" prod))))

(defun process-def-element (dtd name odef cdef production additional)
  (cond ((consp name)
	 (dolist (name (production->name-list name))
	   (process-def-element dtd name odef cdef production additional)))
	((let ((obegin? (canon-optional-tag-definition odef))
	       (oend?   (canon-optional-tag-definition cdef))
	       (incl    (subst :pcdata :cdata		;xxx hack here
			       (production->name-list/2 production)))
	       (excl    nil))
	   (cond ((and (consp additional) (eq (car additional) '+))
		  (setf incl (union incl (production->name-list/2 (cadr additional)))))
		 ((and (consp additional) (eq (car additional) '-))
		  (setf excl (production->name-list/2 (cadr additional))))
		 ((null additional))
		 (t
		  (error "Bogus extra inclusion/exclusion - ~S" additional)))
	   (let ((elm (find-element dtd name t)))
	     (setf (element-include elm) (if (equal incl '(:empty)) nil incl)
		   (element-exclude elm) excl
		   (element-obegin? elm) obegin?
		   (element-oend? elm)   oend?))))))

(defun process-attribute (name type value)
  (declare (ignore value))
  (setq type (production->name-list type))
  (cond ((and (= (length type) 1)
	      (member (car type) '(:cdata)))
	 (list (intern (symbol-name name) :keyword) 't))
	((and (= (length type) 1)
	      (member (car type) '(:number :name :id)))
	 (list name (car type)))
	((list (intern (symbol-name name) :keyword) type)) ))

(defun process-def-attlist (dtd name attlist)
  (cond ((consp name)
	 (dolist (name (production->name-list name))
	   (process-def-attlist dtd name attlist)))
	(t
	 (setf (element-attlist (find-element dtd name t))
	   (mapcar #'(lambda (x) 
		       (process-attribute (first x) (second x) (third x)))
		   attlist)))))

(defun process-dtd-def (dtd def)
  (cond ((null def))
	((eq (car def) 'def-entity)
	 (push (cons (second def) 
                     (resolve-entities-in-string 
                      (string-rod (third def))
                      (dtd-entities dtd)))
	       (dtd-entities dtd)))
	((eq (car def) 'defelement)
	 (process-def-element dtd 
                              (second def) (third def) (fourth def) (fifth def) (sixth def)))
	((eq (car def) 'defattlist)
	 (process-def-attlist dtd (second def) (third def)))
	(t
	 (error "Bogus dtd-def-form ~S" def))))

;;; Uhu! CDATA seems also to been a defined content element. E.g. upon
;;; STYLE in the HTML-4.0 DTD.


;;;; ------------------------------------------------------------------------------------------
;;;;  SGML resources (how is this called in reality?)
;;;;

(defvar *simple-catalog* nil)

(defun open-sgml-resource (name-space &rest more)
  (ecase name-space
    (:system (apply #'open-system-resource more))
    (:public (apply #'open-public-resource more))
    (:cdata  (apply #'make-string-input-stream more)) ))

(defun open-system-resource (filename)
  (open filename :direction :input))

(defun open-public-resource (name &optional system-fallback)
  (open-sgml-resource :system
		      (cdr (or (assoc name *simple-catalog* :test #'string-equal)
			       system-fallback
			       (error "I do not know where to fetch PUBLIC \"~A\"." name)))))

#+nil
(defun slurp-catalog (catalog-url)
  ;; Really dirty implementation
  (setf *simple-catalog* nil)
  (multiple-value-bind (io header) (netlib::open-document-2 catalog-url)
    (declare (ignore header))
    (unwind-protect
        (let ((str (html-glisp::gstream-as-string io)))
          (with-input-from-string (input str)
            (do ((x (read input nil nil) (read input nil nil)))
                ((null x))
              (assert (equal (symbol-name x) "PUBLIC"))
              (let ((name (read input))
                    (file (read input)))
                (assert (stringp name))
                (assert (stringp file))
                (push (cons name (url:merge-url (url:parse-url file) catalog-url))
                      *simple-catalog*)))))
      (g/close io))))

;;;; ------------------------------------------------------------------------------------------

#+(OR)					;seems to be buggy
(defun sgml-resource-as-string (name)
  (with-open-stream (in (apply #'open-sgml-resource name))
    (let ((buffer (g/make-string 32 :adjustable t)))
      (do* ((i 0 j)
	    (j (html-glisp:read-char-sequence buffer in :start 0 :end 32)
               (html-glisp:read-char-sequence buffer in :start i :end (+ i 4000)) ))
	  ((= j i) (subseq buffer 0 j))
        (princ "%") (finish-output)
	(adjust-array buffer (list (+ j 4000))) ))))

(defun sgml-resource-as-string (name)
  (with-output-to-string (bag)
    (with-open-stream (in (apply #'open-sgml-resource name))
      (do ((x (read-char in nil nil) (read-char in nil nil)))
          ((null x))
        (write-char x bag)))))



;;;; ------------------------------------------------------------------------------------------
;;;;  Calculating the resolve information
;;;;

;;; Token data type

(defstruct token
  )

(defstruct (tag (:include token))
  name)

(defstruct (start-tag (:include tag) (:print-function print-start-tag))
  atts)

(defstruct (end-tag (:include tag) (:print-function print-end-tag)))

(defstruct (comment-token (:include token))
  data)

#||
(defstruct (empty-tag 
            (:include tag)
            (:print-function print-empty-tag))
  atts)
||#

(defun print-start-tag (self sink depth)
  (declare (ignore depth))
  (cond (*print-readably*
         (format sink "#.~S" `(MAKE-START-TAG :NAME ,(tag-name self)
                                              :ATTS ,(start-tag-atts self))))
        (t
         (format sink "<~A>" (tag-name self)))))

(defun print-end-tag (self sink depth)
  (declare (ignore depth))
  (cond (*print-readably*
         (format sink "#.~S" `(MAKE-END-TAG :NAME ,(tag-name self))))
        (t
         (format sink "</~A>" (tag-name self)))))

;;; Elements

(defun elm-stag (elm) 
  (make-start-tag :name elm))

(defun elm-etag (elm) 
  (make-end-tag :name elm))

(defun find-dtd-top-elements (dtd)
  (or (dtd-root-elements dtd)
      (setf (dtd-root-elements dtd)
        (let ((includes nil)
              (tags nil))
          (maphash (lambda (key val) 
                     (pushnew key tags)
                     (dolist (k (element-include val)) 
                       (pushnew k includes))) 
                   (dtd-elements dtd))
          (set-difference tags includes)))))

(defun elm-inclusion (dtd x)
  (cond ((eql x :%top)
         (find-dtd-top-elements dtd)) ;;'(:html))
        (t
         (element-include (find-element dtd x)))))

(defun elm-oend? (dtd x)
  (cond ((eql x :%top) 
         nil)
        ((eql x :pcdata)
         t)
        (t
         (element-oend? (find-element dtd x)))))

(defun elm-ostart? (dtd x)
  (if (eql x :%top) 
      nil
    (element-obegin? (find-element dtd x))))

(defun all-elms (dtd)
  (let ((r nil))
    (maphash (lambda (n i)
               (declare (ignore i))
               (push n r))
             (dtd-elements dtd))
    r))

(defun elm-surclusion (dtd e)
  "For a given element 'e' calculate the surclusion, that is the set of all 
   elements, which may contain `e' as direct child."
  (or (gethash e (dtd-elm-surclusion-cache dtd))
      (setf (gethash e (dtd-elm-surclusion-cache dtd))
        (cond ((eql e :html)
               (list :%top))
              (t
               (loop for k in (all-elms dtd)
                   when (member e (elm-inclusion dtd k))
                   collect k))) )))

(defun legal-in-p (dtd s x)
  (cond ((start-tag-p x)
         (member (tag-name x) (elm-inclusion dtd s)))
        ((end-tag-p x)
         (eq s (tag-name x))) ))

;;; Actual calculation of resolve information

(defun raux (dtd s x yet)
  (let ((res nil))
    (cond ((member s yet)
           nil)
          ((legal-in-p dtd s x)
           (list nil))
          (t
           (let (q)
             (dolist (a (elm-inclusion dtd s))
               (when (and (elm-ostart? dtd a) 
                          (setq q (raux dtd a x (cons s yet))))
                 (cond ((and (end-tag-p (car q))
                             (eql (tag-name (car q)) a))
                        '(warn "RAUX: s=~S x=~S yet=~S -> a=~S" s x yet a))
                       (t
                        (pushnew (elm-stag a) res :key #'tag-name)))))
             (when (elm-oend? dtd s)
               (dolist (z (elm-surclusion dtd s))
                 (when (raux dtd z x (cons s yet))
                   (pushnew (elm-etag s) res :key #'tag-name))))
             res)))))

(defun resolve-key (state token)
  (cond ((start-tag-p token)
         (list state :start (tag-name token)))
        ((end-tag-p token)
         (list state :end (tag-name token)))
        (t
         (error "oops in resolve-key ~S ~S" state token))))

(defun calculate-resolve-info (dtd &optional (top-elment :html))
  (setf (dtd-resolve-info dtd)
    (make-hash-table :test #'equal))
  (setf (dtd-elm-surclusion-cache dtd) (make-hash-table :test #'eq))
  (let ()
    (labels ((puta (a b r)
               (cond ((null r) )
                     ((= (length r) 1)
                      (setf (gethash (resolve-key a b) (dtd-resolve-info dtd)) (car r)))
                     (t
                      (warn "Ambiguous : ~S ~S." a b)))))
      (dolist (a (cons ':%top (all-elms dtd)))
        (dolist (b (cons ':%top (all-elms dtd)))
          (let ((bs (elm-stag b))
                (be (elm-etag b)))
            (unless (legal-in-p dtd a bs)
              (puta a bs (raux dtd a bs nil)))
            (unless (legal-in-p dtd a be)
              (puta a be (raux dtd a be nil)))))))) )

(defun resolve (dtd state token)
  (gethash (resolve-key state token) (dtd-resolve-info dtd)))

;;; -------------------------------------------------------------------------------------------

(defun set-equal (x y &rest options)
  (null (apply #'set-exclusive-or x y options)))

(defun elms-eqv (dtd x y)
  ;; zwei elms sind genau dann aequivalent, wenn inclusion und surclusion gleich sind.
  (and (set-equal (elm-inclusion dtd x) (elm-inclusion dtd y))
       (set-equal (elm-surclusion dtd x) (elm-surclusion dtd y))
       (equal (elm-oend? dtd x) (elm-oend? dtd x))
       (equal (elm-ostart? dtd x) (elm-ostart? dtd x))))

(defun eqv-classes (dtd)
  (let ((classes nil))
    (dolist (k (all-elms dtd))
      (do ((q classes (cdr q)))
          ((null q)
           (push (list k) classes))
        (when (elms-eqv dtd k (caar q))
          (setf (car q) (cons k (car q)))
          (return))))
    classes))

;;;; ----------------------------------------------------------------------------------------------------
;;;;  Compiled DTDs
;;;;

;; Since parsing and 'compiling' DTDs is slow, I'll provide for a way
;; to (un)dump compiled DTD to stream.

(defun dump-dtd (dtd sink)
  (let ((*print-pretty* nil)
        (*print-readably* t)
        (*print-circle* t))
    (princ "#." sink)
    (prin1
     `(MAKE-DTD :NAME ',(dtd-name dtd)
                :ELEMENTS (LET ((R (MAKE-HASH-TABLE :TEST #'EQ)))
                               (SETF ,@(let ((q nil))
                                         (maphash (lambda (key value)
                                                    (push `',value q)
                                                    (push `(GETHASH ',key R) q))
                                                  (dtd-elements dtd))
                                         q))
                               R)
                :ENTITIES ',(dtd-entities dtd)
                :RESOLVE-INFO (LET ((R (MAKE-HASH-TABLE :TEST #'EQUAL))) 
                                   (SETF ,@(let ((q nil))
                                             (maphash (lambda (key value)
                                                        (push `',value q)
                                                        (push `(GETHASH ',key R) q))
                                                      (dtd-resolve-info dtd))
                                             q))
                                   R)
                ;; XXX surclusion-cache fehlt
                )
     sink)))

;;XXX
(defun save-html-dtd ()
  (with-open-file (sink "html-dtd.lisp" :direction :output :if-exists :new-version)
    (print `(in-package :sgml) sink)
    (let ((*package* (find-package :sgml)))
      (princ "(SETQ " sink)
      (prin1 'cl-user::*html-dtd* sink)
      (princ " '" sink)
      (dump-dtd cl-user::*html-dtd* sink)
      (princ ")" sink))))

;;; --------------------------------------------------------------------------------
;;;  dumping DTDs


(defun dump-dtd (dtd filename)
  (let ((*foo* dtd))
    (declare (special *foo*))
    (with-open-file (sink (merge-pathnames filename "*.lisp")
                     :direction :output
                     :if-exists :new-version)
      (format sink "(in-package :sgml)(locally (declare (special *foo*))(setq *foo* '#.*foo*))"))
    (compile-file (merge-pathnames filename "*.lisp"))))

(defun undump-dtd (filename)
  (let (*foo*)
    (declare (special *foo*))
    (load (compile-file-pathname (merge-pathnames filename "*.lisp"))
          :verbose nil
          :print nil)
    *foo*))

(defmethod make-load-form ((self dtd) &optional env)
  (declare (ignore env))
  `(make-dtd :name                  ',(dtd-name self)
             :elements              ',(dtd-elements self)
             :entities              ',(dtd-entities self)
             :resolve-info          ',(dtd-resolve-info self)
             :elm-surclusion-cache  ',(dtd-elm-surclusion-cache self)
             :root-elements         ',(dtd-root-elements self)))

(defmethod make-load-form ((self element) &optional env)
  (declare (ignore env))
  `(make-element
    :name    ',(element-name self)
    :include ',(element-include self)
    :exclude ',(element-exclude self)
    :obegin? ',(element-obegin? self)
    :oend?   ',(element-oend? self)
    :attlist ',(element-attlist self)))

(defmethod make-load-form ((self tag) &optional env)
  (declare (ignore env))
  `(make-tag :name ',(tag-name self)))

(defmethod make-load-form ((self start-tag) &optional env)
  (declare (ignore env))
  `(make-start-tag :name ',(tag-name self)
                   :atts ',(start-tag-atts self)))

(defmethod make-load-form ((self end-tag) &optional env)
  (declare (ignore env))
  `(make-end-tag :name ',(tag-name self)))

(defmethod make-load-form ((self comment-token) &optional env)
  (declare (ignore env))
  `(make-comment-token :data ',(comment-token-data self)))

;;;;
#||
(defmethod my-make-load-form ((self dtd))
  (declare (ignore env))
  `(make-dtd :name                  ,(my-make-load-form (dtd-name self))
             :elements              ,(my-make-load-form (dtd-elements self))
             :entities              ,(my-make-load-form (dtd-entities self))
             :resolve-info          ,(my-make-load-form (dtd-resolve-info self))
             :elm-surclusion-cache  ,(my-make-load-form (dtd-elm-surclusion-cache self))
             :root-elements         ,(my-make-load-form (dtd-root-elements self))))

(defmethod my-make-load-form ((self element))
  (declare (ignore env))
  `(make-element
    :name    ,(my-make-load-form (element-name self))
    :include ,(my-make-load-form (element-include self))
    :exclude ,(my-make-load-form (element-exclude self))
    :obegin? ,(my-make-load-form (element-obegin? self))
    :oend?   ,(my-make-load-form (element-oend? self))
    :attlist ,(my-make-load-form (element-attlist self))))

(defmethod my-make-load-form ((self tag))
  (declare (ignore env))
  `(make-tag :name ,(my-make-load-form (tag-name self))))

(defmethod my-make-load-form ((self start-tag))
  (declare (ignore env))
  `(make-start-tag :name ,(my-make-load-form (tag-name self))
                   :atts ,(my-make-load-form (start-tag-atts self))))

(defmethod my-make-load-form ((self end-tag))
  (declare (ignore env))
  `(make-end-tag :name ,(my-make-load-form (tag-name self))))

(defmethod my-make-load-form ((self comment-token))
  (declare (ignore env))
  `(make-comment-token :data ,(my-make-load-form (comment-token-data self))))

(defmethod my-make-load-form ((object cons))
  (let ((yet nil))
    (do ((q object (cdr q)))
        ((atom q)
         (if (null q)
             `(list ,@(reverse yet))
           `(list* ,@(reverse yet) ,(my-make-load-form q))))
      (push (my-make-load-form (car q)) yet))))

(defmethod my-make-load-form ((object string)) 
  object)

(defmethod my-make-load-form ((object vector))
  `(make-array ,(length object)
               :element-type ',(array-element-type object)
               :initial-contents ,(my-make-load-form (coerce object 'list))))

(defmethod my-make-load-form ((object symbol))
  (cond ((keywordp object) object)
        ((eq object nil) nil)
        (t `',object)))

(defmethod my-make-load-form ((object number))
  object)

(defmethod my-make-load-form ((object hash-table))
  `(let ((res (make-hash-table :test ',(hash-table-test object))))
     ,@(let ((todo nil))
         (maphash (lambda (key value)
                    (push `(gethash ,(my-make-load-form key) res) todo)
                    (push (my-make-load-form value) todo))
                  object)
         todo)
     res))

(defun dump-dtd (dtd filename)
  (with-open-file (sink filename
                   :direction :output
                   :if-exists :new-version)
    (with-standard-io-syntax
      (let ((*package* (find-package :cl)))
        (print (my-make-load-form dtd) sink))))
  (values))
||#

#||
;; ACL specific

(defun dump-dtd (dtd filename)
  (with-open-file (sink filename
                   :direction :output
                   :if-exists :new-version
                   :element-type '(unsigned-byte 8))
    (with-standard-io-syntax
      (let ((*package* (find-package :cl)))
        (excl:fasl-write dtd sink))))
  (values))

(defun undump-dtd (filename)
  (first (excl:fasl-read (open filename :element-type '(unsigned-byte 8)))))
||#
