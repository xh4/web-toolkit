(in-package :component)

(defclass component-class (standard-class)
  ((render
    :initarg :render
    :initform nil
    :accessor component-render)
   (render-lambda-list
    :initarg :render-lambda-list
    :initform nil
    :accessor component-render-lambda-list)
   (allowed-tags
    :initarg :allowed-tags
    :initform nil
    :accessor component-allowed-tags)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass
      ((class component-class) (super-class standard-class)) t))

;; TODO: write a macro
(defmethod shared-initialize :around ((class component-class) slot-names
                                      &rest args
                                      &key name direct-slots direct-superclasses location
                                        extra-initargs direct-default-initargs documentation
                                        render
                                        &allow-other-keys)
  (if render
      (let ((lambda-form (car render)))
        (setf render (make-render lambda-form))
        ;; TODO: check render
        ;; (check-render render)
        (let ((render-lambda-list (function-lambda-list render)))
          (setf (slot-value class 'render) render)
          (setf (slot-value class 'render-lambda-list) render-lambda-list)))
      (progn (setf (slot-value class 'render) nil
                   (slot-value class 'render-lambda-list) nil)))
  (if (getf args :name)
      ;; First initialize
      (call-next-method class slot-names
                        :name name
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :location location)
      ;; Rest initialize
      (call-next-method class slot-names
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :extra-initargs extra-initargs
                        :direct-default-initargs direct-default-initargs
                        :documentation documentation
                        :location location)))

(defclass component (html:custom-element)
  ((tag
    :initarg :tag
    :initform nil
    :accessor component-tag)
   (root
    :initarg :root
    :initform nil
    :accessor component-root)
   (children
    :initarg :children
    :initform nil
    :accessor component-children))
  (:metaclass component-class))

(defmethod initialize-instance :after ((component component) &key)
  (unless (component-tag component)
    (error "Tag for component ~A is not specified" component)))

(defmethod component-render ((component component))
  (component-render (class-of component)))

(defmethod component-render-lambda-list ((component component))
  (component-render-lambda-list (class-of component)))

(defmethod component-allowed-tags ((component component))
  (component-allowed-tags (class-of component)))

(defmethod root ((component component))
  (component-root component))

(defmethod children ((component component))
  (component-children component))

(defmethod serialize ((component component) &optional stream)
  (serialize (render component) stream))

(defmacro define-component (name superclasses slots &rest options)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-component-class ,name ,superclasses ,slots ,@options)
       (define-component-macro ,name))))

(defmacro define-component-class (name superclasses slots &rest options)
  (unless (find 'component superclasses)
    (appendf superclasses '(component)))
  (unless (find :metaclass options :key 'first)
    (rewrite-class-option options :metaclass component-class))
  (let ((tag-option (find :tag options :key 'first)))
    (unless tag-option
      (error "Missing tag option when defining component ~A" name))
    (let ((default-tag (second tag-option)))
      ;; TODO: refine this
      (appendf slots
               `((tag :initform ,default-tag)))))
  `(progn
     (defclass ,name ,superclasses
       ,slots
       ,@options)))

(defmacro define-component-macro (component-name)
  `(defun ,component-name (&rest arguments)
     (multiple-value-bind (attributes children)
         (segment-attributes-children arguments)
       (let ((component (make-instance ',component-name
                                       :attributes attributes
                                       :children children)))
         (let ((root (form-element `(,(component-tag component) ,@attributes))))
           (setf (component-root component) root))
         component))))

(defgeneric render (component)
  (:method ((component component))
    (when-let ((render (component-render component)))
      (let ((lambda-list (component-render-lambda-list component)))
        (cond
          ((= 0 (length lambda-list)) (funcall render))
          ((= 1 (length lambda-list)) (funcall render component)))))))

(defgeneric copy-element (element)
  (:method ((list list))
    (mapcar 'copy-element list))
  (:method ((element html:element))
    (let ((class (class-of element)))
      (let ((new-element (allocate-instance class)))
        (dolist (slot (mapcar #'slot-definition-name (class-slots class)))
          (when (slot-boundp element slot)
            (setf (slot-value new-element slot)
                  (slot-value element slot))))
        (setf (dom:children new-element) (copy-element (dom:children element)))
        new-element))))

(defun make-render (lambda-form)
  (unless (eq 'lambda (car lambda-form))
    (error "Expect a LAMBDA form for render, got ~A" lambda-form))
  (let ((lambda-list (cadr lambda-form)))
    (unless (listp lambda-list)
      (error "Expect a LAMBDA form for render, got ~A" lambda-form))
    (let ((component (car lambda-list)))
      (unless component
        (error "Malformed LAMBDA-LIST for render, got ~A" lambda-list))
      (let ((body (cddr lambda-form)))
        (let ((render-lambda-form
               `(lambda ,lambda-list
                  (setf (component-root ,component)
                        (copy-element (component-root ,component)))
                  (setf (component-children ,component)
                        (copy-element (component-children ,component)))
                  (macrolet ((root (&rest arguments)
                               (let ((component ',component))
                                 `(let* ((constructor (html:constructor (component-tag ,component)))
                                         (arguments (list ,@arguments)))
                                    (apply 'html:construct constructor arguments)))))
                    (symbol-macrolet ((root (component-root ,component))
                                      (children (component-children ,component)))
                      ,@body)))))
          (values (eval render-lambda-form) render-lambda-form))))))

(defun form-element (form)
  (let ((tag (first form))
        (arguments (rest form)))
    (let ((constructor (constructor tag)))
      (apply 'construct constructor arguments))))

;; (pprint (nth-value 1 (make-render '(lambda (compoment) (root)))))

;; (funcall (make-render '(lambda (component) (root))) (foo))

(define-component foo ()
  ()
  (:tag :span)
  (:render (lambda (com)
             (root))))
