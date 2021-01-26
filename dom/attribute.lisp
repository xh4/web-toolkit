(in-package :dom)

(defclass attribute ()
  ((name
    :initarg :name
    :initform nil
    :accessor attribute-name)
   (value
    :initarg :value
    :initform nil
    :accessor attribute-value)))

(defclass named-node-map ()
  ((element
    :initarg :element
    :initform nil)
   (attributes
    :initarg :attributes
    :initform nil)))

(defmethod length ((named-node-map named-node-map))
  (cl:length (slot-value named-node-map 'attributes)))

(defmethod item ((named-node-map named-node-map) (index integer))
  (if (>= index (cl:length (slot-value named-node-map 'attributes)))
      nil
      (aref (slot-value named-node-map 'attributes) index)))

(defun get-named-item (named-node-map qualified-name)
  )

(defun get-named-item-ns (named-node-map namespace local-name)
  )

(defun set-named-item (named-node-map attr)
  )

(defun set-named-item-ns (named-node-map attr)
  )

(defun remove-named-item (named-node-map qualified-name)
  )

(defun remove-named-item-ns (named-node-map namespace local-name)
  )

(defclass attr (node)
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
   (name
    :initarg :name
    :initform nil)
   (value
    :initarg :value
    :initform nil
    :accessor value)
   (owner-element
    :initarg :owner-element
    :initform nil
    :reader owner-element)))

(defmethod print-object ((attr attr) stream)
  (print-unreadable-object (attr stream :type t :identity t)
    (with-slots (value) attr
      (format stream "~A=~S" (qualified-name attr) value))))

(defmethod node-type ((attr attr))
  attribute-node)

(defmethod node-value ((attr attr))
  (slot-value attr 'value))

(defmethod text-content ((attr attr))
  (slot-value attr 'value))

(defgeneric name (object)
  (:method ((attr attr))
    ))

(defmethod qualified-name ((attr attr))
  (with-slots (local-name namespace-prefix) attr
    (if namespace-prefix
        (format nil "~A:~A" namespace-prefix local-name)
        local-name)))

(defun create-attribute (document local-name)
  (check-type document (or document null))
  (check-type local-name string)
  ;; TODO: check name
  (when (html-document-p document)
    (setf local-name (string-downcase local-name)))
  (make-instance 'attr
                 :local-name local-name
                 :owner-document document))

(defun create-attribute-ns (document namespace qualified-name)
  (check-type document (or document null))
  (check-type namespace (or string null))
  (check-type qualified-name string)
  (destructuring-bind (namespace prefix local-name)
      (validate-and-extract namespace qualified-name)
    (make-instance 'attr
                   :namespace namespace
                   :namespace-prefix prefix
                   :local-name local-name
                   :owner-document document)))
