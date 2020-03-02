(in-package :dom)

(defclass element (node
                   parent-node
                   non-document-type-child-node
                   child-node)
  ((tag-name
    :initarg :tag-name
    :initform nil
    :reader tag-name)
   (id
    :initarg :id
    :initform nil
    :reader id)
   (class-name
    :initarg :class-name
    :initform nil
    :reader class-name)
   (attributes
    :initarg :attributes
    :initform nil
    :reader attributes)))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (tag-name object) stream)))

(defgeneric has-attributes (element)
  (:method ((element element))
    (not (null (attributes element)))))

(defgeneric get-attribute-names (element)
  (:method ((element element))
    (loop for (name . nil) in (attributes element)
       collect name)))

(defgeneric get-attribute (element name)
  (:method ((element element) name)
    (check-type name (or string symbol))
    (when (symbolp name)
      (setf name (string-downcase (symbol-name name))))
    (loop for (name0 . value) in (attributes element)
       when (equal name name0)
       do (return value))))

(defgeneric set-attribute (element name value)
  (:method ((element element) name value)
    (check-type name (or string symbol))
    (check-type value (or null string))
    (when (null value)
      (return-from set-attribute (remove-attribute element name)))
    (when (symbolp name)
      (setf name (string-downcase (symbol-name name))))
    (with-slots (attributes) element
      (loop with found = nil
         for attribute in attributes
         when (equal name (car attribute))
         do (setf (cdr attribute) value
                  found t)
         finally (unless found
                   (appendf attributes (list (cons name value))))))))

(defgeneric remove-attribute (element name)
  (:method ((element element) name)
    (check-type name (or string symbol))
    (when (symbolp name)
      (setf name (string-downcase (symbol-name name))))
    (with-slots (attributes) element
      (setf attributes (cl:remove name attributes :test 'equal :key 'car)))))

(defgeneric toggle-attribute (element name &optional force)
  (:method ((element element) name &optional force)
    (error "TODO: implementation toggle-attribute")))

(defgeneric has-attribute (element name)
  (:method ((element element) name)
    (check-type name (or string symbol))
    (when (symbolp name)
      (setf name (string-downcase (symbol-name name))))
    (loop for (name0 . nil) in (attributes element)
       when (equal name0 name)
       do (return t))))
