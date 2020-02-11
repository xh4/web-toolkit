(in-package :sax-tests)

(defun first-start-element-event (string)
  (let ((events (cxml:parse-rod string (make-instance 'event-collecting-handler))))
    (find :start-element events :key #'car)))


;;; Attribute handling

(deftest no-default-namespace-for-attributes
  (let* ((evt (first-start-element-event "<x xmlns='http://example.com' a='b'/>"))
	 (attr (car (fifth evt))))
    (values
     (attribute-namespace-uri attr)
     (attribute-local-name attr)))
  nil nil)

(deftest attribute-uniqueness-1
  (handler-case
      (cxml:parse-rod "<x xmlns:a='http://example.com' xmlns:b='http://example.com' a:a='1' b:a='1'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest attribute-uniqueness-2
  (handler-case
      (cxml:parse-rod "<x xmlns:a='http://example.com' xmlns='http://example.com' a:a='1' a='1'/>")
    (error () nil)
    (:no-error (&rest junk)
      (declare (ignore junk))
      t))
  t)

(deftest attribute-uniqueness-3
  (let ((sax:*namespace-processing* nil))
    (handler-case
	(cxml:parse-rod "<x xmlns:a='http://example.com' xmlns:b='http://example.com' a:a='1' b:a='1'/>")
    (error () nil)
    (:no-error (&rest junk)
      (declare (ignore junk))
      t)))
  t)

;;; Namespace undeclaring

(deftest undeclare-default-namespace-1
  (let* ((evts (cxml:parse-rod "<x xmlns='http://example.com'><y xmlns='' a='1'/></x>"
				   (make-instance 'event-collecting-handler)))
	 (start-elt-events (remove :start-element evts :test (complement #'eql) :key #'car))
	 (evt1 (first start-elt-events))
	 (evt2 (second start-elt-events )))
    (values
     (rod= #"http://example.com" (second evt1))
     (second evt2)
     (third evt2)))
  t nil nil)

(deftest undeclare-other-namespace
  (handler-case
      (cxml:parse-rod "<x:x xmlns:x='http://example.com'><x:y xmlns:x='' a='1'/></x:x>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)


;;; Require names otherwise totally out of scope of the xmlns rec to be NcNames for no reason

(deftest pi-names-are-ncnames-when-namespace-processing-1
  (handler-case
      (cxml:parse-rod "<?a:b c?><x/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest pi-names-are-ncnames-when-namespace-processing-2
  (let ((sax:*namespace-processing* nil))
    (handler-case
	(cxml:parse-rod "<?a:b c?><x/>")
      (error () nil)
      (:no-error (&rest junk)
	(declare (ignore junk))
	t)))
  t)

(deftest entity-names-are-ncnames-when-namespace-processing-1
  (handler-case
      (cxml:parse-rod "<!DOCTYPE x [ <!ENTITY y:z 'foo'> ]><x>&y:z;</x>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest entity-names-are-ncnames-when-namespace-processing-2
  (handler-case
      (cxml:parse-rod "<!DOCTYPE x [ <!ENTITY y:z 'foo'> ]><x/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest entity-names-are-ncnames-when-namespace-processing-3
  (let ((sax:*namespace-processing* nil))
    (handler-case
	(cxml:parse-rod "<!DOCTYPE x [ <!ENTITY y:z 'foo'> ]><x>&y:z;</x>")
      (error () nil)
      (:no-error (&rest junk)
	(declare (ignore junk))
	t)))
  t)

(deftest entity-names-are-ncnames-when-namespace-processing-4
  (let ((sax:*namespace-processing* nil))
    (handler-case
	(cxml:parse-rod "<!DOCTYPE x [ <!ENTITY y:z 'foo'> ]><x/>")
      (error () nil)
      (:no-error (&rest junk)
	(declare (ignore junk))
	t)))
  t)

;;; Inclusion of xmlns attributes

(deftest xmlns-attr-include-1
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (length attrs))
  1)

(deftest xmlns-attr-discard-1
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* nil)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (length attrs))
  0)

;;; Namespace of xmlns attributes

(deftest xmlns-attr-ns-uri-1
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* nil)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (attribute-namespace-uri (car attrs)))
  nil)

(deftest xmlns-attr-ns-uri-2
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* nil)
	 (evt (first-start-element-event "<x xmlns:foo='http://example.com'/>"))
	 (attrs (fifth evt)))
    (attribute-namespace-uri (car attrs)))
  nil)

(deftest xmlns-attr-ns-uri-3
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* t)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (attribute-namespace-uri (car attrs)))
  nil)

(deftest xmlns-attr-ns-uri-4
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* t)
	 (evt (first-start-element-event "<x xmlns:foo='http://example.com'/>"))
	 (attrs (fifth evt)))
    (rod= #"http://www.w3.org/2000/xmlns/" (attribute-namespace-uri (car attrs))))
  t)

(deftest xmlns-attr-ns-local-name-1
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* nil)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (attribute-local-name (car attrs)))
  nil)

(deftest xmlns-attr-ns-local-name-2
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* nil)
	 (evt (first-start-element-event "<x xmlns:foo='http://example.com'/>"))
	 (attrs (fifth evt)))
    (attribute-local-name (car attrs)))
  nil)

(deftest xmlns-attr-ns-local-name-3
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* t)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (attribute-local-name (car attrs)))
  nil)

(deftest xmlns-attr-ns-local-name-4
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* t)
	 (evt (first-start-element-event "<x xmlns:foo='http://example.com'/>"))
	 (attrs (fifth evt)))
    (rod= #"foo" (attribute-local-name (car attrs))))
  t)

(deftest xmlns-attr-qname-1
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* nil)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (rod= #"xmlns" (attribute-qname (car attrs))))
  t)

(deftest xmlns-attr-qname-2
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* nil)
	 (evt (first-start-element-event "<x xmlns:foo='http://example.com'/>"))
	 (attrs (fifth evt)))
    (rod= #"xmlns:foo" (attribute-qname (car attrs))))
  t)

(deftest xmlns-attr-qname-4
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* t)
	 (evt (first-start-element-event "<x xmlns='http://example.com'/>"))
	 (attrs (fifth evt)))
    (rod= #"xmlns" (attribute-qname (car attrs))))
  t)

(deftest xmlns-attr-qname-4
  (let* ((sax:*namespace-processing* t)
	 (sax:*include-xmlns-attributes* t)
	 (sax:*use-xmlns-namespace* t)
	 (evt (first-start-element-event "<x xmlns:foo='http://example.com'/>"))
	 (attrs (fifth evt)))
    (rod= #"xmlns:foo" (attribute-qname (car attrs))))
  t)


;;; Predefined Namespaces

(deftest redefine-xml-namespace-1
  (handler-case
      (cxml:parse-rod "<x xmlns:xml='http://www.w3.org/XML/1998/namespace'/>")
    (error () nil)
    (:no-error (&rest junk)
      (declare (ignore junk))
      t))
  t)

(deftest redefine-xml-namespace-2
  (handler-case
      (cxml:parse-rod "<x xmlns:xml='http://example.com/wrong-uri'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest redefine-xml-namespace-3
  (handler-case
      (cxml:parse-rod "<x xmlns:wrong='http://www.w3.org/XML/1998/namespace'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest redefine-xml-namespace-4
  (handler-case
      (cxml:parse-rod "<x xmlns:wrong='http://www.w3.org/XML/1998/namespace'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest redefine-xmlns-namespace-1
  (handler-case
      (cxml:parse-rod "<x xmlns:xmlns='http://www.w3.org/2000/xmlns/'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest redefine-xmlns-namespace-2
  (handler-case
      (cxml:parse-rod "<x xmlns:xmlns='http://example.com/wrong-ns'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest redefine-xmlns-namespace-3
  (handler-case
      (cxml:parse-rod "<x xmlns:wrong='http://www.w3.org/2000/xmlns/'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)

(deftest redefine-xmlns-namespace-4
  (handler-case
      (cxml:parse-rod "<x xmlns='http://www.w3.org/2000/xmlns/'/>")
    (error () t)
    (:no-error (&rest junk)
      (declare (ignore junk))
      nil))
  t)
