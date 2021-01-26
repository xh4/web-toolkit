(in-package :dom)

(defclass element (node
                   parent-node
                   non-document-type-child-node
                   child-node)
  ((namespace
    :initarg :namespace
    :initform nil
    :reader namespace-uri)
   (namespace-prefix
    :initarg :namespace-prefix
    :initform nil
    :reader prefix)
   (local-name
    :initarg :local-name
    :initform nil
    :reader local-name)
   (attributes
    :initarg :attributes
    :initform (make-array 5 :adjustable t :fill-pointer 0))))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (tag-name object) stream)))

(defmethod node-type ((element element))
  element-node)

(defmethod node-name ((element element))
  (html-uppercased-qualified-name element))

;; TODO: create-an-element
;; https://dom.spec.whatwg.org/#concept-create-element
(defun create-an-element (document local-name namespace &optional prefix)
  (check-type document (or document null))
  (check-type local-name string)
  (check-type namespace (or string null))
  (check-type prefix (or string null))
  (make-instance 'element
                 :document document
                 :namespace namespace
                 :namespace-prefix prefix
                 :local-name local-name))

(defun create-element (document local-name)
  ;; TODO: check local-name
  ;; https://dom.spec.whatwg.org/#dom-document-createelement

  (when (html-document-p document)
    (setf local-name (string-downcase local-name)))

  (let ((namespace (if (or (html-document-p document)
                           ;; ("equal" "application/xhtml+xml" (content-type document))
                           )
                       html-namespace
                       nil)))
    (create-an-element document local-name namespace nil)))

;; TODO: validate and extrace
;; https://dom.spec.whatwg.org/#validate-and-extract
(defun validate-and-extract (namespace qualified-name)
  (list namespace nil qualified-name))

(defun create-element-ns (document namespace qualified-name)
  (destructuring-bind (namespace prefix local-name)
      (validate-and-extract namespace qualified-name)
    (create-an-element document local-name namespace prefix)))

(defgeneric tag-name (element)
  (:method ((element element))
   (html-uppercased-qualified-name element)))

(defgeneric qualified-name (element)
  (:method ((element element))
   (with-slots (local-name namespace-prefix) element
     (if namespace-prefix
         (format nil "~A:~A" namespace-prefix local-name)
       local-name))))

(defgeneric html-uppercased-qualified-name (element)
  (:method ((element element))
    (let ((qualified-name (qualified-name element)))
      (when (equal html-namespace (namespace-uri element))
        (setf qualified-name (string-upcase qualified-name)))
      qualified-name)))

(defgeneric id (element)
  (:method ((element element))
    (get-attribute element "id")))

(defgeneric class-name (element)
  (:method ((element element))
    (get-attribute element "class")))

;; TODO: class-list
(defgeneric class-list (element)
  (:method ((element element))
    ))

(defgeneric attributes (element)
  (:method ((element element))
    (make-instance 'named-node-map
                   :element element
                   :attributes (slot-value element 'attributes))))

(defgeneric has-attributes (element)
  (:method ((element element))
    (plusp (cl:length (slot-value element 'attributes)))))

(defgeneric get-attribute-names (element)
  (:method ((element element))
    (loop for attribute across (slot-value element 'attributes)
       collect (qualified-name attribute))))

(defun get-an-attribute (element qualified-name)
  (check-type qualified-name string)
  (when (and (equal html-namespace (namespace-uri element))
             (owner-document element)
             (html-document-p (owner-document element)))
    (setf qualified-name (string-downcase qualified-name)))
  (with-slots (attributes) element
    (loop for attribute across attributes
       when (equal qualified-name (qualified-name attribute))
       do (return attribute))))

(defun get-an-attribute-ns (element namespace local-name)
  (check-type namespace (or string null))
  (check-type local-name string)
  (if (= 0 (length namespace))
      (setf namespace nil)
      (with-slots (attributes) element
        (loop for attribute across attributes
           when (and (equal namespace (namespace-uri attribute))
                     (equal local-name (local-name attribute)))
           do (return attribute)))))

(defgeneric get-attribute (element qualified-name)
  (:method ((element element) qualified-name)
    (when-let ((attribute (get-an-attribute element qualified-name)))
      (value attribute))))

(defgeneric get-attribute-ns (element namespace local-name)
  (:method ((element element) namespace local-name)
    (when-let ((attribute (get-an-attribute-ns element namespace local-name)))
      (value attribute))))

(defgeneric set-attribute (element qualified-name value)
  (:method ((element element) qualified-name value)
    (check-type value string)

    ;; TODO: check name
    ;; https://dom.spec.whatwg.org/#dom-element-setattribute

    ;; TODO: tweak qualified-name

    (if-let ((attribute (get-an-attribute element qualified-name)))
      (setf (value attribute) value)
      (with-slots (attributes) element
        (let ((attribute (make-instance 'attr
                                        :local-name qualified-name
                                        :value value
                                        :document (owner-document element))))
          (vector-push-extend attribute attributes))))))

(defgeneric set-attribute-ns (element namespace qualified-name value)
  (:method ((element element) namespace qualified-name value)
    (check-type value string)
    (destructuring-bind (namespace prefix local-name)
        (validate-and-extract namespace qualified-name)
      (if-let ((attribute (get-an-attribute-ns element namespace qualified-name)))
        (setf (value attribute) value)
        (with-slots (attributes) element
          (let ((attribute (make-instance 'attr
                                          :namespace namespace
                                          :namespace-prefix prefix
                                          :local-name local-name
                                          :value value
                                          :document (owner-document element))))
            (vector-push-extend attribute attributes)))))))

(defgeneric set-attribute-node (element attr)
  (:method ((element element) (attr attr))
    (when (owner-element attr)
      (error "InUseAttributeError"))

    (let ((old-attr (get-attribute-node-ns
                     element
                     (namespace-uri attr)
                     (local-name attr))))
      (when (eq attr old-attr)
        (return-from set-attribute-node attr))
      (when old-attr
        (setf (slot-value old-attr 'owner-element) (owner-element attr)
              (value old-attr) (value attr))
        (with-slots (attributes) element
          (loop for attribute across attributes
             for index from 0
             when (eq attribute old-attr)
             do (return (setf (aref attributes index) attr))))
        (setf (slot-value attr 'owner-element) (owner-element old-attr))
        (setf (slot-value old-attr 'owner-element) nil))
      old-attr)))

(defgeneric remove-attribute (element qualified-name)
  (:method ((element element) qualified-name)
    (when-let ((attribute (get-an-attribute element qualified-name)))
      (with-slots (attributes) element
        (let ((index (position attribute attributes)))
          (loop for i from index upto (- (cl:length attributes) 2)
             do (setf (aref attributes i) (aref attributes (1+ i)))
             finally (decf (fill-pointer attributes)))))
      attribute)))

(defgeneric remove-attribute-ns (element namespace local-name)
  (:method ((element element) namespace local-name)
    (when-let ((attribute (get-an-attribute-ns element namespace local-name)))
      (with-slots (attributes) element
        (let ((index (position attribute attributes)))
          (loop for i from index upto (- (cl:length attributes) 2)
             do (setf (aref attributes i) (aref attributes (1+ i)))
             finally (decf (fill-pointer attributes)))))
      attribute)))

(defgeneric toggle-attribute (element qualified-name &optional force)
  (:method ((element element) qualified-name &optional (force nil force-given-p))
    ;; TODO: check name

    (when (and (equal html-namespace (namespace-uri element))
               (owner-document element)
               (html-document-p (owner-document element)))
      (setf qualified-name (string-downcase qualified-name)))

    (with-slots (attributes) element
      (let ((attr (loop for attr across attributes
                     when (equal qualified-name (qualified-name attr))
                     do (return attr))))
        (if attr
            (if (or (not force-given-p) (not force))
                (progn (remove-attribute element qualified-name) nil)
                t)
            (if (or (not force-given-p) force)
                (let ((attr (make-instance 'attr
                                           :local-name qualified-name
                                           :value ""
                                           :document (owner-document element))))
                  (vector-push-extend attr attributes)
                  t)
                nil))))))

(defgeneric has-attribute (element qualified-name)
  (:method ((element element) qualified-name)
    (when (get-an-attribute element qualified-name)
      t)))

(defgeneric has-attribute-ns (element namespace local-name)
  (:method ((element element) namespace local-name)
    (when (= 0 (length namespace))
      (setf namespace nil))
    (when (get-an-attribute-ns element namespace local-name)
      t)))

(defgeneric get-attribute-node (element qualified-name)
  (:method ((element element) qualified-name)
    (get-an-attribute element qualified-name)))

(defgeneric get-attribute-node-ns (element namespace local-name)
  (:method ((element element) namespace local-name)
    (get-an-attribute-ns element namespace local-name)))

(defun set-an-attribute (element attr)
  (when (or (null (owner-element attr))
            (not (eq (owner-element attr) element)))
    (error "InUseAttributeError"))
  (let ((old-attr (get-an-attribute-ns element (namespace-uri attr) (local-name attr))))
    (if (eq old-attr attr)
        attr)
    (with-slots (attributes) element
      (if old-attr
          (let ((index (position old-attr attributes)))
            (setf (aref attributes index) attr))
          (vector-push-extend attr attributes)))
    old-attr))

(defgeneric set-attribute-node (element attr)
  (:method ((element element) (attr attribute))
    (set-an-attribute element attr)))

(defgeneric set-attribute-node-ns (element attr)
  (:method ((element element) (attr attribute))
    (set-an-attribute element attr)))

(defgeneric remove-attribute-node (element attr)
  (:method ((element element) (attr attribute))
    (with-slots (attributes) element
      (if-let ((index (position attr attributes)))
        (loop for i from index upto (- (cl:length attributes) 2)
           do (setf (aref attributes i) (aref attributes (1+ i)))
           finally (decf (fill-pointer attributes)))
        (error "NotFoundError")))))
