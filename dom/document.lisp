(in-package :dom)

(defclass document (node
                    non-element-parent-node
                    document-or-shadow-root
                    parent-node)
  ((encoding
    :initarg :encoding
    :initform nil)
   (content-type
    :initarg :content-type
    :initform nil
    :reader content-type)
   (type
    :initarg :type
    :initform nil)
   (mode
    :initarg :mode
    :initform nil)))

;; TODO: character-set
;; https://dom.spec.whatwg.org/#dom-document-characterset
(defgeneric character-set (document)
  (:method ((document document))
    ))

(defgeneric doctype (document)
  (:method ((document document))
    (loop for child in (node-list-nodes (child-nodes document))
       when (typep child 'document-type)
       do (return child))))

(defgeneric document-element (document)
  (:method ((document document))
    (first-child document)))

(defgeneric import-node (document node &optional deep)
  (:method ((document document) (node node) &optional deep)
    ))

(defgeneric adopt-node (document node)
  (:method ((document document) (node node))
    ))

(defclass xml-document (document)
  ((type
    :initarg :type
    :initform "xml")))

(defclass document-type (node child-node)
  ((name
    :initarg :name
    :initform nil
    :reader name)
   (public-id
    :initarg :public-id
    :initform nil
    :reader public-id)
   (system-id
    :initarg :system-id
    :initform nil
    :reader system-id)))

(defmethod print-object ((document-type document-type) stream)
  (print-unreadable-object (document-type stream :type t :identity t)
    (format stream "~S" (name document-type))))

;; TODO: check slots
(defmethod initialize-instance :after ((document-type document-type) &key)
  )

(defun create-document (namespace qualified-name doctype)
  (let ((document (make-instance 'xml-document))
        (element nil))
    ;; TODO: create element
    ;; https://dom.spec.whatwg.org/#dom-domimplementation-createdocument
    (when doctype
      (append-child document doctype))
    (when element
      (append-child document element))
    (let ((content-type (switch (namespace :test 'equal)
                          (html-namespace "application/xhtml+xml")
                          (svg-namespace "image/svg+xml")
                          (t "application/xml"))))
      (setf (slot-value document 'content-type) content-type))
    document))

(defun create-html-document (title)
  (check-type title (or string null))
  (let ((document (make-instance 'document
                                 :type "html"
                                 :content-type "text/html")))
    (append-child document (make-instance 'document-type
                                          :name "html"
                                          :document document))
    (let ((html (create-an-element document "html" html-namespace)))
      (append-child document html)
      (let ((head (create-an-element document "head" html-namespace)))
        (append-child html head)
        (when title
          (let ((title-el (create-an-element document "title" html-namespace)))
            (append-child head title-el)
            (append-child title-el (create-text-node document title)))))
      (let ((body (create-an-element document "body" html-namespace)))
        (append-child html body)))
    document))

(defmethod node-type ((document document))
  document-node)

(defmethod node-name ((document document))
  "#document")

(defmethod owner-document ((document document))
  nil)

(defun xml-document-p (document)
  (equal "xml" (slot-value document 'type)))

(defun html-document-p (document)
  (not (xml-document-p document)))

(defclass document-fragment ()
  ())

(defmethod node-type ((document-fragment document-fragment))
  document-fragment-node)

(defmethod node-name ((document-fragment document-fragment))
  "#document-fragment")
