(in-package :component)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass component-class (reactive-class)
    ((render
      :initarg :render
      :initform nil)
     (%render
      :initform nil
      :accessor component-render)
     (render-lambda-list
      :initarg :render-lambda-list
      :initform nil
      :accessor component-render-lambda-list))))

(defmethod shared-initialize :after ((class component-class) slot-names
                                     &key render &allow-other-keys)
  (declare (ignore slot-names))
  (if render
      (let ((lambda-form (car render)))
        (setf render (make-render lambda-form))
        (let ((render-lambda-list (function-lambda-list render)))
          (setf (slot-value class '%render) render)
          (setf (slot-value class 'render-lambda-list) render-lambda-list)))
      (progn (setf (slot-value class '%render) nil
                   (slot-value class 'render-lambda-list) nil)))
  (loop for object in (rx:object-propagation class)
     when (typep object 'component)
     do (incf (component-version object))))

(defmethod (setf slot-value-using-class) :around (value (class component-class) object slot)
  (typecase slot
    (symbol slot)
    (slot-definition (setf slot (slot-definition-name slot))))
  (if (eq 'version slot)
      (call-next-method)
      (prog1
          (rx:without-propagation (call-next-method))
        (when (and (slot-boundp object 'version)
                   (find slot (class-direct-slots class)
                         :key 'slot-definition-name))
          (incf (component-version object))))))

(defmethod rx:react ((component-1 component) (component-2 component))
  (incf (component-version component-1)))

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
  (:metaclass component-class))

(defmethod initialize-instance :after ((component component) &key)
  (when (next-method-p)
    (call-next-method)))

(defmethod component-render ((component component))
  (component-render (class-of component)))

(defmethod component-render-lambda-list ((component component))
  (component-render-lambda-list (class-of component)))

(defgeneric compute-component-class (component)
  (:method ((component component))
    (flet ((class-name-for-component-class (class)
             (format nil "~(~A~)-~(~A~)"
                     (package-name (symbol-package (cl:class-name class)))
                     (symbol-name (cl:class-name class)))))
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
                                :children children
                                slot-attributes)))
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
          (rx:without-propagation
            ;; Set tag name
            (setf (slot-value component 'dom:tag-name) (dom:tag-name root))
            ;; Set class
            ;; FIXME: handle special characters in class names
            (add-class root (format nil "~(~A~)" (class-name (class-of component))))
            ;; Merge attributes
            (loop for name in (dom:get-attribute-names component)
               for value = (dom:get-attribute component name)
               if (equal "class" name)
               do (add-class root value)
               else do (dom:set-attribute root name value)))
          (setf (component-root component) root)))
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
                (rx:without-propagation
                  ,@body))))
        (values (eval render-lambda-form) render-lambda-form)))))
