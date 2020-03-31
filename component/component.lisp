(in-package :component)

(defclass component (html:custom-element reactive-object)
  ((root
    :initarg :root
    :initform nil)
   (children
    :initarg :children
    :initform nil)
   (version
    :initarg :version
    :initform 0))
  (:metaclass component-class)
  #+lispworks
  (:optimize-slot-access nil))

(defmethod shared-initialize :around ((component component) slot-names
                                     &key &allow-other-keys)
  (declare (ignore slot-names))
  (let ((*component-initializing* t))
    (call-next-method)))

(defmethod react ((component component) (child component))
  (incf (slot-value component 'version)))

(defmethod react ((component component) (variable variable))
  (incf (slot-value component 'version)))

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
       ,@(when-let ((render (second (find :render options :key 'first))))
           `((define-component-render ,name ,render)))
       ,@(when-let ((style (second (find :style options :key 'first))))
           `((define-component-style ,name ,style)))
       (define-component-constructor ,name))))

(defmacro define-component-class (component-name superclasses slots &rest options)
  (unless (find 'component superclasses)
    (appendf superclasses '(component)))
  (unless (find :metaclass options :key 'first)
    (rewrite-class-option options :metaclass component-class))
  #+lispworks
  (rewrite-class-option options :optimize-slot-access nil)
  (setf options (remove :render options :key 'first)
        options (remove :style options :key 'first))
  `(progn
     (defclass ,component-name ,superclasses
       ,slots
       ,@options)
     (ensure-finalized (find-class ',component-name))))

(defmacro define-component-render (component-name lambda-form)
  (unless (eq 'lambda (car lambda-form))
    (error "Expect a LAMBDA form for render, got ~A" lambda-form))
  (let ((lambda-list (cadr lambda-form)))
    (unless (listp lambda-list)
      (error "Expect a LAMBDA form for render, got ~A" lambda-form))
    (unless (<= (length lambda-list) 1)
      (error "Malformed LAMBDA-LIST for render, got ~A" lambda-list))
    (let ((body (cddr lambda-form)))
      `(define-render-method (,(car lambda-list) ,component-name)
         (without-propagation
           (with-variable-capturing (,(car lambda-list))
             ,@body))))))

(defmacro define-component-style (component-name lambda-form)
  (unless (eq 'lambda (car lambda-form))
    (error "Expect a LAMBDA form for style, got ~A" lambda-form))
  (let ((lambda-list (cadr lambda-form)))
    (unless (listp lambda-list)
      (error "Expect a LAMBDA form for style got ~A" lambda-form))
    (when (> (length lambda-list) 0)
      (error "Malformed LAMBDA-LIST for style, got ~A" lambda-list))
    (let ((body (cddr lambda-form)))
      `(define-component-class-style-method ,component-name
         (declare (ignore component))
         ,@body))))

(defmacro define-component-constructor (component-name)
  (let* ((constructor-name (format nil "~A-CONSTRUCTOR" component-name))
         (constructor-symbol (intern constructor-name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,constructor-symbol (constructor)
         ((component-class :initform ',component-name)))
       (defun ,component-name (&rest arguments)
         (let ((constructor (make-instance ',constructor-symbol)))
           (apply 'construct constructor arguments))))))
