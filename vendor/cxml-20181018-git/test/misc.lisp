;;;
;;; When I'll grow up, I'll be a complete test suite.

(deftest utf-8
    (flet ((doit (from below)
	     (loop for code from from below below do
		  (when (and (code-char code)
			     (not (eql code #xfffe))
			     (not (eql code #xffff)))
		    (let* ((a (if (< code #x10000)
				  (format nil "abc~C" (code-char code))
				  (let* ((x (- code #x10000))
					 (lo (ldb (byte 10 0) x))
					 (hi (ldb (byte 10 10) x)))
				    (format nil "abc~C~C"
					    (code-char (logior #xD800 hi))
					    (code-char 
					     (logior #xDC00 lo))))))
			   (b (cxml:utf8-string-to-rod
			       (cxml:rod-to-utf8-string
				a))))
		      (unless (string= a b)
			(format t "FAIL: ~S ~A ~A~%"
				(code-char code)
				(map 'vector #'char-code a)
				(map 'vector #'char-code b))))))))
      (doit 32 #xD800)
      (doit #x10000 char-code-limit)
      (values)))

;; <4824D34F.5000002@acceleration.net>
(deftest chtml-encoding
  (with-output-to-string (stream)
   (let ((doc (dom:create-document
             'rune-dom:implementation
             nil nil
             (dom:create-document-type
              'rune-dom:implementation
              "html"
              "-//W3C//DTD XHTML 1.0 Transitional//EN"
              "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"))))
     (dom:map-document (chtml:make-character-stream-sink stream)
           doc
           :include-xmlns-attributes nil
           :include-doctype :full-internal-subset))))
