(in-package :component)

(defclass component ()
  ((id
    :initarg :id
    :initform nil
    :accessor id)
   (tag
    :initarg :tag
    :initform nil
    :accessor component-tag)
   (class
    :initarg :class
    :initform nil
    :accessor component-class)
   (children
    :initarg :children
    :initform nil
    :accessor children)))

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
  (let* ((component-name name)
         (tag-slot-definition-form
          (find "TAG" slots
                :key (lambda (slot-definition-form)
                       (when-let (slot-symbol (first slot-definition-form))
                         (symbol-name slot-symbol)))
                :test 'equal))
         (tag-option-form (find :tag options :key 'first)))
    (if tag-slot-definition-form
        (let ((initform (getf (rest tag-slot-definition-form) :initform)))
          (unless initform
            (if tag-option-form
                (setf (getf (rest tag-slot-definition-form) :initform)
                      (second tag-option-form))
                (error "Components must specify TAG via slot definition or tag option"))))
        (if tag-option-form
            (push `(tag
                    :initarg :tag
                    :initform ,(second tag-option-form)
                    :accessor component-name) slots)
            (error "Components must specify TAG via slot definition or tag option")))
    `(progn
       (defclass ,component-name (,@super-components component)
         ,slots)
       (defun ,component-name (&rest arguments)
         (multiple-value-bind (attributes children)
             (segment-attributes-children arguments)
           (let ((component (apply 'make-instance ',component-name attributes)))
             (loop for child in children
                do (append-child component
                                 (typecase child
                                   (component child)
                                   (string (html:text child))
                                   (t (error "Can't add ~A of type ~A as a child of component"
                                             child (type-of child))))))
             component))))))
