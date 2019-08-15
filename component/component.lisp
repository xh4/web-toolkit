(in-package :component)

(defclass component ()
  ((id
    :initarg :id
    :initform nil
    :accessor id)
   (children
    :initarg :children
    :initform nil
    :accessor children))
  (:metaclass component-class))

(defmethod print-object ((component component) stream)
  (print-unreadable-object (component stream :type t)
    (format stream "#~D" (id component))))

(defmethod initialize-instance :after ((component component) &key)
  (let ((id (parse-integer (subseq (symbol-name (gensym "G")) 1))))
    (setf (id component) id)))

(defun append-child (parent child)
  (appendf (children parent) (list child)))

;; attributes => plist
;; body => list
(defun segment-attributes-children (form)
  (let* ((body (loop for rest on form by #'cddr
                  unless (and (keywordp (car rest)) (cdr rest))
                  return rest))
         (attributes (ldiff form body)))
    (values attributes body)))

(defmacro define-component (name super-components slots &rest options)
  `(progn
     (define-component-class ,name ,super-components ,slots)
     (process-component-class-options (find-class ',name) ',options)
     (define-component-constructor ,name)
     (find-class ',name)))

(defmacro define-component-class (name super-components slots &rest options)
  (declare (ignore options))
  `(defclass ,name (,@super-components component)
     ,slots
     (:metaclass component-class)))

(defmacro define-component-constructor (name)
  `(defun ,name (&rest arguments)
     (multiple-value-bind (attributes children)
         (segment-attributes-children arguments)
       (let ((component (apply 'make-instance ',name attributes)))
         (loop for child in children
            do (append-child component
                             (typecase child
                               (component child)
                               (string (html:text child))
                               (t (error "Can't add ~A of type ~A as a child of component"
                                         child (type-of child))))))
         component))))

(defun process-component-class-options (class options)
  (let ((option-groups (group-by:group-by options :value #'identity)))
    (loop for option-group in option-groups
       for group-name = (car option-group)
       for group-options = (cdr option-group)
       do (process-component-class-option-group
            class
            group-name
            group-options))))

(defgeneric process-component-class-option-group (component-class group-name options)
  (:method (component-class group-name options)
    (loop for option in options
       for option-name = (car option)
       for option-value = (cdr option)
       do (process-component-class-option component-class option-name option-value))))

(defgeneric process-component-class-option (component-class option-name option-value)
  (:method (component-class option-name option-value)
    (declare (ignore option-value))
    (error "Don't know how to process component option ~A for ~A" option-name component-class)))
