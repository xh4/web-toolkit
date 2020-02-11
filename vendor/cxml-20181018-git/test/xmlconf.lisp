(defpackage xmlconf
  (:use :cl :runes)
  (:export #:run-all-tests))
(in-package :xmlconf)

(defun get-attribute (element name)
  (rod-string (dom:get-attribute element name)))

(defparameter *bad-tests*
    '(;; TS14
      ;; http://lists.w3.org/Archives/Public/public-xml-testsuite/2002Mar/0001.html
      "ibm-valid-P28-ibm28v02.xml"
      "ibm-valid-P29-ibm29v01.xml"
      "ibm-valid-P29-ibm29v02.xml"))

(defun test-class (test)
  (cond
    ((not (and (let ((version (get-attribute test "RECOMMENDATION")))
                 (cond
                   ((or (equal version "") ;XXX
                        (equal version "XML1.0")
                        (equal version "NS1.0"))
                     (cond
                       ((equal (get-attribute test "NAMESPACE") "no")
                         (format t "~A: test applies to parsers without namespace support, skipping~%"
                                 (get-attribute test "URI"))
                         nil)
                       (t
                         t)))
                   ((equal version "XML1.1")
                     ;; not supported
                     nil)
                   (t
                     (warn "unrecognized RECOMMENDATION value: ~S" version)
                     nil)))
               (not (member (get-attribute test "ID") *bad-tests* :test 'equal))))
      nil)
    ((equal (get-attribute test "TYPE") "valid") :valid)
    ((equal (get-attribute test "TYPE") "invalid") :invalid)
    ((equal (get-attribute test "TYPE") "not-wf") :not-wf)
    (t nil)))

(defun test-pathnames (directory test)
  (let* ((sub-directory
          (loop
              for parent = test then (dom:parent-node parent)
              for base = (get-attribute parent "xml:base")
              until (plusp (length base))
              finally (return (merge-pathnames base directory))))
         (uri (get-attribute test "URI"))
         (output (get-attribute test "OUTPUT")))
    (values (merge-pathnames uri sub-directory)
            (when (plusp (length output))
              (merge-pathnames output sub-directory)))))

(defmethod serialize-document ((document t))
  (dom:map-document (cxml:make-octet-vector-sink :canonical 2)
		    document
		    :include-doctype :canonical-notations
		    :include-default-values t))

(defun file-contents (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (let ((result
           (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence result s )
      result)))

(defun dribble-tests (parser-fn directory)
  (let ((base (slot-value (asdf:find-system :cxml) 'asdf::relative-pathname)))
    (with-open-file (*standard-output*
		     (merge-pathnames "XMLCONF" base)
		     :direction :output
		     :external-format :iso-8859-1
		     :if-exists :supersede)
      (run-all-tests parser-fn directory))))

(defvar *parser-fn* 'sax-test)

(defun sax-test (filename handler &rest args)
  (apply #'cxml:parse-file filename handler :recode nil args))

(defun klacks-test (filename handler &rest args)
  (klacks:with-open-source
      (s (apply #'cxml:make-source (pathname filename) args))
    (klacks:serialize-source s handler)))

(defun run-all-tests (parser-fn directory)
  (let* ((*parser-fn* parser-fn)
	 (pathname (merge-pathnames "xmlconf.xml" directory))
         (builder (rune-dom:make-dom-builder))
         (xmlconf (cxml:parse-file pathname builder :recode nil))
         (ntried 0)
         (nfailed 0)
         (nskipped 0)
         ;; XXX someone found it funny to include invalid URIs in the
         ;; test suite.  And no, in "invalid" not "not-wf".
         (puri:*strict-parse* nil))
    (dom:do-node-list (test (dom:get-elements-by-tag-name xmlconf "TEST"))
      (let ((description
             (apply #'concatenate
		    'string
		    (map 'list
		      (lambda (child)
			(if (dom:text-node-p child)
			    (rod-string (dom:data child))
			    ""))
		      (dom:child-nodes test))))
            (class (test-class test)))
        (cond
          (class
            (incf ntried)
            (multiple-value-bind (pathname output)
                (test-pathnames directory test)
              (princ (enough-namestring pathname directory))
              (unless (probe-file pathname)
                (error "file not found: ~A" pathname))
              (with-simple-restart (skip-test "Skip this test")
                (unless (run-test class pathname output description)
                  (incf nfailed))
                (fresh-line))))
          (t
            (incf nskipped)))))
    (format t "~&~D/~D tests failed; ~D test~:P were skipped"
            nfailed ntried nskipped)))

(defmethod run-test :around (class pathname output description &rest args)
  (declare (ignore class pathname output args))
  (handler-case
      (call-next-method)
    (serious-condition (c)
      (format t " FAILED:~%  ~A~%[~A]~%" c description)
      nil)))

(defmethod run-test ((class null) pathname output description &rest args)
  (declare (ignore description))
  (let ((document (apply *parser-fn*
                         pathname
                         (rune-dom:make-dom-builder)
                         args)))
    ;; If we got here, parsing worked.  Let's try to serialize the same
    ;; document.  (We do the same thing in canonical mode below to check the
    ;; content model of the output, but that doesn't even catch obvious
    ;; errors in DTD serialization, so even a simple here is an
    ;; improvement.)
    (apply *parser-fn* pathname (cxml:make-rod-sink) args)
    (cond
      ((null output)
        (format t " input"))
      ((equalp (file-contents output) (serialize-document document))
        (format t " input/output"))
      (t
        (let ((error-output (make-pathname :type "error" :defaults output)))
          (with-open-file (s error-output
                           :element-type '(unsigned-byte 8)
                           :direction :output
                           :if-exists :supersede)
            (write-sequence (serialize-document document) s))
          (error "well-formed, but output ~S not the expected ~S~%"
                 error-output output))))
    t))

(defmethod run-test
    ((class (eql :valid)) pathname output description &rest args)
  (assert (null args))
  (and (progn
         (format t " [not validating:]")
         (run-test nil pathname output description :validate nil))
       (progn
         (format t " [validating:]")
         (run-test nil pathname output description :validate t))))

(defmethod run-test
    ((class (eql :invalid)) pathname output description &rest args)
  (assert (null args))
  (and (progn
         (format t " [not validating:]")
         (run-test nil pathname output description :validate nil))
       (handler-case
           (progn
             (format t " [validating:]")
             (funcall *parser-fn*
		      pathname
		      (rune-dom:make-dom-builder)
		      :validate t)
             (error "validity error not detected")
             nil)
         (cxml:validity-error ()
           (format t " invalid")
           t))))

(defmethod run-test
    ((class (eql :not-wf)) pathname output description &rest args)
  (assert (null args))
  (handler-case
      (progn
         (format t " [not validating:]")
	(funcall *parser-fn*
		 pathname
		 (rune-dom:make-dom-builder)
		 :validate nil)
	(error "well-formedness violation not detected")
      nil)
    #+fixme-stp-test
    (error ()
      (format t " unexpected-error")
      t)
    (cxml:well-formedness-violation ()
      (format t " not-wf")
      t))
  (handler-case
      (progn
	(format t " [validating:]")
	(funcall *parser-fn*
		 pathname
		 (rune-dom:make-dom-builder)
		 :validate t)
	(error "well-formedness violation not detected")
      nil)
    #+fixme-stp-test
    (error ()
      (format t " unexpected-error")
      t)
    (cxml:well-formedness-violation ()
      (format t " not-wf")
      t)
    (cxml:validity-error ()
      ;; das erlauben wir mal auch, denn valide => wf
      (format t " invalid")
      t)))

#+(or)
(xmlconf::run-all-tests 'xmlconf::sax-test
			"/home/david/2001/XML-Test-Suite/xmlconf/")

#+(or)
(xmlconf::run-all-tests 'xmlconf::klacks-test
			"/home/david/2001/XML-Test-Suite/xmlconf/")
