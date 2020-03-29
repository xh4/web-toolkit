(in-package :component)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass component-class (reactive-class)
    ((render
      :initarg :render
      :initform nil
      :accessor component-render)
     (style
      :initarg :style
      :initform nil
      :accessor component-style))))

(defclass component (html:custom-element reactive-object)
  ((root
    :initarg :root
    :initform nil
    :accessor component-root)
   (children
    :initarg :children
    :initform nil
    :accessor component-children)
   (version
    :initarg :version
    :initform 0
    :accessor component-version))
  (:metaclass component-class)
  #+lispworks
  (:optimize-slot-access nil))

(defmethod initialize-instance :after ((component component) &key)
  (when (next-method-p)
    (call-next-method)))

(defmethod component-render ((component component))
  (component-render (class-of component)))

(defmethod component-render-lambda-list ((component component))
  (function-lambda-list (component-render (class-of component))))

(defmethod component-style ((component component))
  (component-style (class-of component)))

(defmethod shared-initialize :around ((class component-class) slot-names
                                      &rest args &key render style &allow-other-keys)
  (declare (ignore slot-names))
  (when render
    (let ((lambda-form (car render)))
      (setf (getf args :render) (make-render lambda-form))))
  (when style
    (let ((lambda-form (car style)))
      (setf (getf args :style) (make-styler lambda-form))))
  (apply #'call-next-method class slot-names args))

(defmethod shared-initialize :after ((class component-class) slot-names
                                       &key &allow-other-keys)
  (declare (ignore slot-names))
  (loop for object in (object-propagation class)
     when (typep object 'component)
     do (incf (component-version object))))

(defvar *component-initializing* nil)

(defmethod shared-initialize :around ((class component) slot-names
                                     &key &allow-other-keys)
  (declare (ignore slot-names))
  (let ((*component-initializing* t))
    (call-next-method)))

(defmethod (setf slot-value-using-class) :around (value (class component-class) object slot)
  (declare (ignore value))
  (typecase slot
    (symbol slot)
    (slot-definition (setf slot (slot-definition-name slot))))
  (if *component-initializing*
      (without-propagation (call-next-method))
      (if (eq 'version slot)
          (call-next-method)
          (prog1
              (without-propagation (call-next-method))
            (when (find slot (class-direct-slots class)
                        :key 'slot-definition-name)
              (incf (component-version object)))))))

(defmethod react ((component component) (child component))
  (incf (component-version component)))

(defmethod react ((component component) (variable variable))
  (incf (component-version component)))

(defgeneric compute-component-class (component)
  (:method ((component component))
    (flet ((class-name-for-component-class (class)
             (format nil "~(~A~)" (symbol-name (cl:class-name class)))))
      (let ((classes (compute-class-precedence-list (class-of component))))
        (loop for class in classes
           until (eq class (find-class 'component))
           collect (class-name-for-component-class class)
           into classes
           finally (return (reverse classes)))))))

(defmethod serialize ((component component) &optional stream)
  (serialize (render component) stream))

(defclass constructor ()
  ((component-class
    :initarg :component-class
    :initform nil
    :reader constructor-component-class)))

(defgeneric construct (constructor &rest arguments)
  (:method ((constructor constructor) &rest arguments)
    (multiple-value-bind (attributes children)
        (segment-attributes-children arguments)
      (let* ((slot-initargs (flatten
                             (mapcar 'slot-definition-initargs
                                     (class-slots
                                      (find-class
                                       (constructor-component-class constructor))))))
             (slot-attributes '())
             (root-attributes '()))
        (loop for (name value) on attributes by #'cddr
           if (find (symbol-name name) slot-initargs :key 'symbol-name :test 'equal)
           do (appendf slot-attributes `(,name ,value))
           else do (appendf root-attributes `((,name . ,value))))
        (loop for (_name . _value) in root-attributes
           for name = (string-downcase (symbol-name _name))
           for value = (if (eq _value t)
                           ""
                           (typecase _value
                             (null nil)
                             (string _value)
                             (list (format nil "~{~A~^ ~}" _value))
                             (t (format nil "~A" _value))))
           when value
           collect `(,name . ,value) into attributes
           finally (setf root-attributes attributes))
        (let ((component (apply 'make-instance
                                (constructor-component-class constructor)
                                :attributes root-attributes
                                slot-attributes)))
          (loop for child in children
             when (typep child 'reactive-object)
             do (add-dependency component child)
             do (dom:append-child component child))
          component)))))

(defmacro define-component (name superclasses slots &rest options)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-component-class ,name ,superclasses ,slots ,@options)
       (define-component-constructor ,name))))

(defmacro define-component-class (name superclasses slots &rest options)
  (unless (find 'component superclasses)
    (appendf superclasses '(component)))
  (unless (find :metaclass options :key 'first)
    (rewrite-class-option options :metaclass component-class))
  #+lispworks
  (rewrite-class-option options :optimize-slot-access nil)
  `(progn
     (defclass ,name ,superclasses
       ,slots
       ,@options)
     (ensure-finalized (find-class ',name))))

(defmacro define-component-constructor (component-name)
  (let* ((constructor-name (format nil "~A-CONSTRUCTOR" component-name))
         (constructor-symbol (intern constructor-name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,constructor-symbol (constructor)
         ((component-class :initform ',component-name)))
       (defun ,component-name (&rest arguments)
         (let ((constructor (make-instance ',constructor-symbol)))
           (apply 'construct constructor arguments))))))

(defgeneric add-class (element class)
  (:method ((string string) class)
    (let ((classes (split-sequence #\Space string)))
      (unless (find class classes :test 'equal)
        (push class classes))
      (format nil "~{~A~^ ~}" classes)))
  (:method ((nothing null) class)
    class)
  (:method ((element html:element) new-class)
    (let ((class (dom:get-attribute element "class")))
      (dom:set-attribute element "class" (add-class class new-class)))))

(defgeneric render (component)
  (:method ((component component))
    (if-let ((render (component-render component)))
      (let ((lambda-list (component-render-lambda-list component)))
        (let ((root (cond
                      ((= 0 (length lambda-list)) (funcall render))
                      ((= 1 (length lambda-list)) (funcall render component)))))
          (unless (typep root '(or html:element null))
            (error "Render of ~A returned ~A which is not of type ELEMENT"
                   component root))
          (without-propagation
            ;; Set tag name
            (setf (slot-value component 'dom:tag-name) (dom:tag-name root))
            ;; Set class
            ;; FIXME: handle special characters in class names
            (loop for class in (compute-component-class component)
               do (add-class root class))
            ;; Merge attributes
            (loop for name in (dom:get-attribute-names component)
               for value = (dom:get-attribute component name)
               if (equal "class" name)
               do (add-class root value)
               else do (dom:set-attribute root name value))
            (setf (component-root component) root))))
      (error "Render is not specified on component ~A" component))))

(defun make-render (lambda-form)
  (unless (eq 'lambda (car lambda-form))
    (error "Expect a LAMBDA form for render, got ~A" lambda-form))
  (let ((lambda-list (cadr lambda-form)))
    (unless (listp lambda-list)
      (error "Expect a LAMBDA form for render, got ~A" lambda-form))
    (unless (<= (length lambda-list) 1)
      (error "Malformed LAMBDA-LIST for render, got ~A" lambda-list))
    (let ((body (cddr lambda-form)))
      (let ((render-lambda-form
             `(lambda ,lambda-list
                (without-propagation
                  (with-variable-capturing (,(car lambda-list))
                    ,@body)))))
        (values (eval render-lambda-form) render-lambda-form)))))

(defun css-properties ()
  (loop for class in (class-direct-subclasses (find-class 'css:property))
     for name = (class-name class)
     when (fboundp name)
     collect name))

(define-condition css-property ()
  ((property
    :initarg :property
    :initform nil)))

(define-condition css-rule ()
  ((rule
    :initarg :rule
    :initform nil)))

(defmacro with-css-as-signal (&body body)
  (let ((properties (css-properties)))
    `(flet ,(loop for property in properties
               collect `(,property (value)
                                   (let ((property (funcall (symbol-function ',property) value)))
                                     (signal 'css-property :property property))))
       (macrolet ((css:rule (selector &body body)
                    `(let ((properties '()))
                       (handler-bind ((css-property (lambda (c)
                                                      (push (slot-value c 'property)
                                                            properties)
                                                      (change-class c 'condition))))
                         ,@body
                         (let ((rule (apply (symbol-function 'css:rule)
                                            ,selector
                                            properties)))
                           (signal 'css-rule :rule rule))))))
         ,@body))))

(defun make-styler (lambda-form)
  (unless (eq 'lambda (car lambda-form))
    (error "Expect a LAMBDA form for style, got ~A" lambda-form))
  (let ((lambda-list (cadr lambda-form)))
    (unless (listp lambda-list)
      (error "Expect a LAMBDA form for style got ~A" lambda-form))
    (when (> (length lambda-list) 0)
      (error "Malformed LAMBDA-LIST for style, got ~A" lambda-list))
    (let ((body (cddr lambda-form)))
      (let ((styler-lambda-form
             `(lambda ,lambda-list
                (let ((properties '())
                      (rules '()))
                  (handler-bind ((css-property (lambda (c)
                                                 (push (slot-value c 'property)
                                                       properties)))
                                 (css-rule (lambda (c)
                                                 (push (slot-value c 'rule)
                                                       rules))))
                    (with-css-as-signal
                      ,@body))
                  (nreverse rules)
                  (push (make-instance 'css:style-rule
                                       :selector (format nil ".~(~A~)" )
                                       :declarations (reverse properties))
                        rules)))))
        (values (eval styler-lambda-form) styler-lambda-form)))))
