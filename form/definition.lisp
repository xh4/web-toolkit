(in-package :form)

(defclass form-definition ()
  ((name
    :initarg :name
    :initform nil
    :accessor form-name)
   (classes
    :initarg :classes
    :initform '(form)
    :accessor form-classes)
   (fields
    :initarg :fields
    :initform nil
    :accessor form-fields)))

(defmethod print-object ((form-definition form-definition) stream)
  (print-unreadable-object (form-definition stream :type t :identity t)
    (format stream "~A" (form-name form-definition))))

(defgeneric form-field (form field-name)
  (:method ((form-definition form-definition) field-name)
    (loop for field-definition in (form-fields form-definition)
       when (eq (field-name field-definition) field-name)
       do (return field-definition))))

(defclass field-definition ()
  ((type
    :initarg :type
    :initform nil
    :accessor field-type)
   (name
    :initarg :name
    :initform nil
    :accessor field-name)
   (class
    :initarg :class
    :initform nil
    :accessor field-class)
   (arguments
    :initarg :arguments
    :initform nil
    :accessor field-arguments)))

(defmethod print-object ((field-definition field-definition) stream)
  (print-unreadable-object (field-definition stream :type t :identity t)
    (format stream "~A" (field-name field-definition))))

(defun compute-form-class-precedence-list (class)
  (let ((classes (ensure-list class)))
    (let ((temporary-class (closer-mop:ensure-class 'temporary-form-class
                                                    :metaclass 'com::component-class
                                                    :direct-superclasses classes)))
      (loop for class in (rest (closer-mop:compute-class-precedence-list temporary-class))
         when (subtypep class 'form)
         collect class))))

(defmacro define-form (name classes fields &rest options)
  (declare (ignore options))
  (unless (find 'form classes)
    (appendf classes '(form)))
  (let ((form-definition (make-instance 'form-definition
                                        :name name
                                        :classes classes)))
    (process-field-definition-forms form-definition fields)
    (let ((form-name name))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; Form definition
         (defparameter ,form-name ,(form-definition-make-form form-definition))
         ;; Form component
         (define-form-component ,form-name ,classes)
         ;; Form constructor
         (define-form-constructor ,form-name)
         ;; Return it
         ,form-name))))

(defun process-field-definition-forms (form-definition field-definition-forms)
  (let ((field-definitions (loop for field-definition-form in field-definition-forms
                              collect (process-field-definition-form
                                       form-definition
                                       field-definition-form))))
    (setf (form-fields form-definition) field-definitions)))

(defun process-field-definition-form (form-definition field-definition-form)
  (let ((field-name (make-keyword (first field-definition-form)))
        (field-type (getf (rest field-definition-form) :type))
        (field-arguments (loop for (name value) on (rest field-definition-form) by 'cddr
                            unless (member name '(:type))
                            append (list name value)))
        (field-class nil))
    (appendf field-arguments (list :name field-name))
    ;; Find field class
    (let ((form-classes (compute-form-class-precedence-list (form-classes form-definition))))
      (loop for form-class in form-classes
         for field-class* = (form-field-class-for-type (class-name form-class)
                                                      field-type)
         when field-class*
         do
           (setf field-class field-class*)
           (return)
         finally
           (error "Unable to find class for field with type ~A, search through classes ~A"
                  field-type
                  (mapcar 'class-name form-classes))))
    (when (symbolp field-class)
      (setf field-class (find-class field-class)))
    ;; Check field arguments
    (loop for (name) on field-arguments by 'cddr
       unless (loop for slot in (closer-mop:class-slots field-class)
                 for slot-initarg = (first (closer-mop:slot-definition-initargs slot))
                 when (eq slot-initarg name)
                 do (return t))
       do (error "Unknown argument ~A for ~A when defining ~A"
                 name
                 (class-name field-class)
                 field-definition-form))
    (make-instance 'field-definition
                   :name field-name
                   :type field-type
                   :class (class-name field-class)
                   :arguments field-arguments)))

(defun form-definition-make-form (form-definition)
  `(make-instance 'form-definition
                  :name ',(form-name form-definition)
                  :classes ',(form-classes form-definition)
                  :fields (list
                           ,@(mapcar 'field-definition-make-form
                                     (form-fields form-definition)))))

(defun field-definition-make-form (field-definition)
  `(make-instance 'field-definition
                  :name ,(field-name field-definition)
                  :type ,(field-type field-definition)
                  :class ',(field-class field-definition)
                  :arguments ',(field-arguments field-definition)))

(defun make-form-slot-definitions (form-definition)
  (loop for field-definition in (form-fields form-definition)
     collect (make-form-slot-definition form-definition field-definition)))

(defun make-form-slot-definition (form-definition field-definition)
  (with-slots (name class arguments) field-definition
    `(,(ensure-symbol (symbol-name name))
       :initarg ,name
       :initform (make-instance ',class ,@arguments))))

(defmacro define-form-component (form-name classes)
  (let ((form-definition (eval form-name)))
    (let ((slot-definitions (make-form-slot-definitions form-definition)))
      `(define-component ,form-name ,classes
         ,slot-definitions))))

(defmacro define-form-constructor (form-name)
  (let* ((form-definition (eval form-name))
         (field-names (loop for field-definition in (form-fields form-definition)
                         collect (field-name field-definition))))
    (with-gensyms (form)
      `(defun ,form-name (&key ,@(mapcar 'ensure-symbol field-names))
         (let ((,form (make-instance ',form-name)))
           ,@(loop for field-name in field-names
                for field-symbol = (ensure-symbol field-name)
                collect `(setf (form-value ,form ,field-name) ,field-symbol))
           ,form)))))
