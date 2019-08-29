(in-package :form)

(define-component form ()
  ((fields
    :initarg :fields
    :initform nil
    :accessor form-fields)))

(defmethod shared-initialize ((form form) slot-names &rest initargs)
  (call-next-method)
  (let ((field-definitions (form-field-definitions form)))
    (loop for field-definition in field-definitions
       for field-name = (field-name field-definition)
       for field-slot = (form-field-slot form field-name)
       for field = (slot-value form (closer-mop:slot-definition-name field-slot))
       do
         (setf (field-form field) form)
         (appendf (form-fields form) (list field)))))

(defgeneric form-field-type-p (form-class field-type))

(defmethod form-field-type-p (form-class field-type)
  (when (symbolp field-type)
    (when (find-method #'form-field-type-p nil
                       `((eql ,form-class) (eql ,(make-keyword field-type))) nil)
      (form-field-type-p form-class (make-keyword field-type)))))

(defmethod form-field-type-p ((form-class (eql 'form)) (type (eql :text))) t)
(defmethod form-field-type-p ((form-class (eql 'form)) (type (eql :email))) t)
(defmethod form-field-type-p ((form-class (eql 'form)) (type (eql :password))) t)

(defmethod form-name ((form form))
  (class-name (class-of form)))

(defmethod form-field ((form form) field-name)
  (loop for field in (form-fields form)
     when (eq field-name (field-name field))
     do (return field)))

(defmethod form-field ((form form) (field-name string))
  (loop for field in (form-fields form)
     when (string-equal field-name (symbol-name (field-name field)))
     do (return field)))

(defgeneric form-field-class-for-type (form-class field-type))

(defmethod form-field-class-for-type (form-class field-type)
  (when (symbolp field-type)
    (when (find-method #'form-field-class-for-type nil
                       `((eql ,form-class) (eql ,(make-keyword field-type))) nil)
      (form-field-class-for-type form-class (make-keyword field-type)))))

(defmethod form-field-class-for-type ((form-class (eql 'form))
                                      (type (eql :text))) 'text-field)
(defmethod form-field-class-for-type ((form-class (eql 'form))
                                      (type (eql :email))) 'email-field)
(defmethod form-field-class-for-type ((form-class (eql 'form))
                                      (type (eql :password))) 'password-field)

(defun form-field-slot (form field-name)
  (loop for slot in (closer-mop:class-slots (class-of form))
     for slot-name = (closer-mop:slot-definition-name slot)
     when (equal (symbol-name slot-name)
                 (symbol-name field-name))
     do (return slot)))

(defun form-definition (form)
  (symbol-value (form-name form)))

(defun form-field-definitions (form)
  (form-fields (form-definition form)))

(defun form-field-definition (form field-name)
  (loop for field-definition in (form-field-definitions form)
     when (eq (field-name field-definition)
              field-name)
     do (return field-definition)))

(defgeneric form-value (form field-name)
  (:method (form field-name)
    (if-let (field (form-field form field-name))
      (field-value field))))

(defgeneric (setf form-value) (field-value form field-name)
  (:method ((field-value field) form field-name)
    (if-let (slot (form-field-slot form field-name))
      (setf (slot-value form (closer-mop:slot-definition-name slot))
            field-value)))
  (:method (field-value form field-name)
    (when-let ((field (form-field form field-name)))
      (setf (field-value field) field-value))))

(define-render form (fields)
  (html:form :method "POST"
             :enctype "application/x-www-form-urlencoded"
   (mapcar 'render fields)))
