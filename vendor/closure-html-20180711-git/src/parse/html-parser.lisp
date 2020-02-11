(in-package :closure-html)

;;; FIXME: I liked the old SLURP-CATALOG code better than the LOOP below.
;;; (Except for the use of NETLIB and URI, which we don't have here.)

#||

(defun slurp-catalog (catalog-url)
  ;; Really dirty implementation
  (setf *simple-catalog* nil)
  (multiple-value-bind (io header) (netlib::open-document-2 catalog-url)
    (declare (ignore header))
    (unwind-protect
        (let ((str (glisp::gstream-as-string io)))
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

(format T "~&;; Parsing DTD~% ")
(sgml:slurp-catalog (url:parse-url "file://closure/resources/dtd/catalog"))
(setf cl-user::*html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))
(format T "~&;; done~%")

||#

(defparameter sgml::*simple-catalog*
  (let ((base
        (make-pathname
	 :name nil
	 :type nil
	 :defaults (merge-pathnames
		    "resources/"
		    (asdf:component-relative-pathname
		     (asdf:find-system :closure-html))))))
    (loop
       :for (name . filename)
       :in '(("-//W3O//DTD W3 HTML 3.0//EN" . "dtd/HTML-3.0")
	     ("NETSCAPE-Bookmark-file-1" . "dtd/NETSCAPE-Bookmark-file-1")
	     ("-//W3C//ENTITIES Special//EN//HTML" . "dtd/Entities-Special")
	     ("-//W3C//ENTITIES Symbols//EN//HTML" . "dtd/Entities-Symbols")
	     ("-//W3C//ENTITIES Latin1//EN//HTML" . "dtd/Entities-Latin1")
	     ("-//W3C//DTD HTML 4.0 Frameset//EN" . "dtd/DTD-HTML-4.0-Frameset")
	     ("-//W3C//DTD HTML 4.0//EN" . "dtd/DTD-HTML-4.0")
	     ("-//W3C//DTD HTML 4.0 Transitional//EN" . "dtd/DTD-HTML-4.0-Transitional"))
       :collect (cons name (merge-pathnames filename base)))))

(defparameter *html-dtd*
  (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))

(defun parse-xstream (input handler)
  (setf (sgml::a-stream-scratch input)
	(make-array #.(* 2 4096) :element-type 'runes:rune))
  (sgml::setup-code-vector input :utf-8)
  (let* ((dtd *html-dtd*)
	 (sgml::*unmungle-attribute-case* t)
	 (r (sgml:sgml-parse dtd input))
	 (pt (sgml::post-mortem-heuristic dtd r)))
    (if handler
	(serialize-pt pt handler)
	pt)))

(defun parse (input handler)
  (etypecase input
    (xstream
     (parse-xstream input handler))
    (rod
     #-rune-is-integer (setf input (string-rod input))
     (let ((xstream (make-rod-xstream input)))
;;;        (setf (xstream-name xstream)
;;; 	     (make-stream-name
;;; 	      :entity-name "main document"
;;; 	      :entity-kind :main
;;; 	      :uri nil))
       (parse-xstream xstream handler)))
    ((and array (not string))
     (parse (make-octet-input-stream input) handler))
    #+rune-is-integer
    (string
     (let ((bytes
	    (make-array (length input) :element-type '(unsigned-byte 8))))
       (map-into bytes #'char-code input)
       (parse bytes handler)))
    (pathname
     (with-open-file (s input :element-type '(unsigned-byte 8))
       (parse s handler)))
    (stream
     (let ((xstream (make-xstream input :speed 8192)))
;;;        (setf (xstream-name xstream)
;;; 	     (make-stream-name
;;; 	      :entity-name "main document"
;;; 	      :entity-kind :main
;;; 	      :uri (pathname-to-uri
;;; 		    (merge-pathnames (or pathname (pathname input))))))
       (parse-xstream xstream handler)))))

(defun serialize-pt-attributes (plist recode)
  (loop
     for (name value) on plist by #'cddr
     unless
       ;; better don't emit as HAX what would be bogus as SAX anyway
       (let ((s (string name))
             (prefix "xmlns:"))
         (or (string-equal s "xmlns")
             (string-equal s prefix :end1 (min (length s) (length prefix)))))
     collect
     (let* ((n #+rune-is-character (coerce (symbol-name name) 'rod)
	       #-rune-is-character (symbol-name name))
	    (v (etypecase value
		 (symbol (coerce (string-downcase (symbol-name value)) 'rod))
		 (rod (funcall recode value))
		 (string (coerce value 'rod)))))
       (hax:make-attribute n v t))))

(defun serialize-pt (document handler
		     &key (name "HTML") public-id system-id (documentp t))
  (let* ((recodep (or #+rune-is-integer (hax:%want-strings-p handler)))
	 (recode
	  (if recodep
	      (lambda (rod)
		(if (typep rod 'rod)
		    (rod-to-utf8-string rod)
		    rod))
	      #'identity)))
    (when documentp
      (hax:start-document handler name public-id system-id))
    (labels ((recurse (pt)
	       (cond
		 ((eq (gi pt) :pcdata)
		  (hax:characters handler (funcall recode (pt-attrs pt))))
		 (t
		  (let* ((name (symbol-name (pt-name pt)))
			 (name
			  #+rune-is-character (coerce name 'rod)
			  #-rune-is-character
			  (if recodep name (string-rod name)))
			 (attrs
			  (serialize-pt-attributes (pt-attrs pt) recode)))
		    (hax:start-element handler name attrs)
		    (mapc #'recurse (pt-children pt))
		    (hax:end-element handler name))))))
      (recurse document))
    (when documentp
      (hax:end-document handler))))

(defclass pt-builder (hax:abstract-handler)
  ((current :initform nil :accessor current)
   (root :initform nil :accessor root)))

#-rune-is-character
(defmethod hax:%want-strings-p ((handler pt-builder))
  nil)

(defun make-pt-builder ()
  (make-instance 'pt-builder))

(defmethod hax:start-document ((handler pt-builder) name pubid sysid)
  (declare (ignore name pubid sysid))
  nil)

(defun unserialize-pt-attributes (attrs)
  (loop
     for a in attrs
     collect (intern (string-upcase (hax:attribute-name a)) :keyword)
     collect (hax:attribute-value a)))

(defmethod hax:start-element ((handler pt-builder) name attrs)
  (let* ((parent (current handler))
	 (this (sgml::make-pt/low
		:name (intern (string-upcase name) :keyword)
		:attrs (unserialize-pt-attributes attrs)
		:parent parent)))
    (setf (current handler) this)
    (if parent
	(push this (pt-children parent))
	(setf (root handler) this))))

(defmethod hax:characters ((handler pt-builder) data)
  (push (sgml::make-pt/low
	 :name :pcdata
	 :attrs data
	 :parent (current handler))
	(pt-children (current handler))))

(defmethod hax:comment ((handler pt-builder) data)
  ;; zzz haven't found out what the representation of comments is...
  data)

(defmethod hax:end-element ((handler pt-builder) name)
  (let ((current (current handler)))
    (setf (pt-children current) (nreverse (pt-children current)))
    (setf (current handler) (pt-parent current))))

(defmethod hax:end-document ((handler pt-builder))
  (root handler))
