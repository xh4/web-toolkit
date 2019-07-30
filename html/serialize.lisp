(in-package :html)

(defclass sink (hax:abstract-handler)
  ((stack
    :initform nil
    :accessor stack)
   (stream
    :initarg :stream
    :initform nil
    :accessor sink-stream)))

(defmethod hax:start-document ((sink sink) name public-id system-id)
  (declare (ignore name public-id system-id))
  (write-string "<!DOCTYPE html>" (sink-stream sink)))

(defmethod hax:end-document ((sink sink)))

(defmethod hax:start-element ((sink sink) name attributes)
  (let* ((key (find-symbol (string-upcase name) :keyword))
	 (elt
	  (and key (sgml::find-element closure-html::*html-dtd* key nil nil)))
	 (attlist (and elt (sgml::element-attlist elt))))
    (push (cons name elt) (stack sink))
    (write-char #\< (sink-stream sink))
    (write-string name (sink-stream sink))
    (dolist (a attributes)
      (let* ((aname (hax:attribute-name a))
	     (akey (find-symbol (string-upcase aname) :keyword))
	     (att (and akey (assoc akey attlist)))
	     (values (second att)))
	(write-char #\space (sink-stream sink))
	(write-string aname (sink-stream sink))
	(unless (and att (listp values) (eq (car att) (car values)))
	  (write-char #\= (sink-stream sink))
	  (write-char #\" (sink-stream sink))
	  (let ((value (hax:attribute-value a)))
	    (when (uri-attribute-p name aname)
	      (setf value (escape-uri-attribute value)))
	    (write-attribute-string value sink))
	  (write-char #\" (sink-stream sink)))))
    (write-char #/> (sink-stream sink))))

(defun uri-attribute-p (ename aname)
  (find (string-downcase aname)
	(cdr (find (string-downcase ename)
		   '((#"a"          #"href" #"name")
		     (#"area"       #"href")
		     (#"link"       #"href")
		     (#"img"        #"src" #"longdesc" #"usemap")
		     (#"object"     #"classid" #"codebase" #"data" #"usemap")
		     (#"q"          #"cite")
		     (#"blockquote" #"cite")
		     (#"inl"        #"cite")
		     (#"del"        #"cite")
		     (#"form"       #"action")
		     (#"input"      #"src" #"usemap")
		     (#"head"       #"profile")
		     (#"base"       #"href")
		     (#"script"     #"src"  ;; #"for"
		      ))
		   :key #'car
		   :test #'string=))
	:test #'string=))

(defun escape-uri-attribute (x)
  (with-output-to-string (s)
    (loop
       for c across x
       for code = (char-code c)
       do
	 (if (< code 128)
	     (write-char c s)
	     (format s "%~2,'0X" code)))))

(defmethod hax:end-element
    ((sink sink) name)
  (let* ((prev (pop (stack sink)))
	 (prev-name (car prev))
	 (elt (cdr prev)))
    (unless (string= prev-name name)
      (error "output does not nest: expected ~A but got ~A"
             name prev-name))
    (unless (and elt (null (sgml::element-include elt)))
      (write-string "</" (sink-stream sink))
      (write-string name (sink-stream sink))
      (write-string ">" (sink-stream sink)))))

(defmethod hax:characters ((sink sink) data)
  (let ((stream (sink-stream sink)))
    (if (find (caar (stack sink)) '("script" "style") :test 'equalp)
	(write-string data (sink-stream sink))
	(loop for c across data do (write-datachar-readable c (sink-stream sink))))))

(defun write-attribute-string (str sink)
  (let ((stream (sink-stream sink)))
    (loop
       for i from 1
       for c across str
       do
	 (cond ((char= c #/&)
		(if (and (< i (length str)) (char= (char str i) #/{))
		    (write-char c stream)
                    (write-string '#.(string "&amp;") stream)))
	       ((char= c #/\") (write-string '#.(string "&quot;") stream))
	       ((char= c #/U+000A) (write-string '#.(string "&#10;") stream))
	       ((char= c #/U+000D) (write-string '#.(string "&#13;") stream))
	       (t (write-char c stream))))))

(defun write-datachar (char stream)
  (cond ((char= char #/&) (write-string '#.(string "&amp;") stream))
        ((char= char #/<) (write-string '#.(string "&lt;") stream))
        ((char= char #/>) (write-string '#.(string "&gt;") stream))
        ((char= char #/\") (write-string '#.(string "&quot;") stream))
        ((char= char #/U+0009) (write-string '#.(string "&#9;") stream))
        ((char= char #/U+000A) (write-string '#.(string "&#10;") stream))
        ((char= char #/U+000D) (write-string '#.(string "&#13;") stream))
        (t (write-char char stream))))

(defun write-datachar-readable (char stream)
  (cond ((char= char #\&) (write-string '#.(string "&amp;") stream))
        ((char= char #\<) (write-string '#.(string "&lt;") stream))
        ((char= char #\>) (write-string '#.(string "&gt;") stream))
        ((char= char #/\") (write-string '#.(string "&quot;") stream))
        ((char= char #\U+000D) (write-string '#.(string "&#13;") stream))
        (t (write-char char stream))))

(defun serialize (root &optional (stream nil))
  (let ((handler nil))
    (labels ((serialize-to-stream (root stream)
               (setf handler (make-instance 'sink :stream stream))
               (typecase root
                 (document
                  (hax:start-document handler nil nil nil)
                  (dom:do-node-list (child (dom:child-nodes root))
                    (visit child))
                  (hax:end-document handler))
                 ((or element text)
                  (visit root))
                 (t (error "Can't serialize ~A of type ~A" root (type-of root)))))
             (visit (node)
               (ecase (dom:node-type node)
                 (:element
                  (let ((name (dom:tag-name node))
                        (attributes (compute-attributes node)))
                    (hax:start-element handler name attributes)
                    (walk node)
                    (hax:end-element handler name)))
                 (:text
                  (hax:characters handler (dom:data node)))))
             (walk (node)
               (dom:do-node-list (child (dom:child-nodes node))
                 (visit child))))
      (if (eq t stream)
          (progn (serialize-to-stream root *standard-output*) nil)
          (typecase stream
            (null (with-output-to-string (stream)
                    (serialize-to-stream root stream)))
            (t (progn (serialize-to-stream root *standard-output*) nil)))))))

(defun compute-attributes (element)
  (let ((results '()))
    (dom:do-node-list (a (dom:attributes element))
      (push
       (hax:make-attribute (dom:name a)
                           (dom:value a)
                           (dom:specified a))
       results))
    (reverse results)))
