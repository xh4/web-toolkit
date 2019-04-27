(in-package :component)

(defclass component ()
  ((id
    :initarg :id
    :initform nil
    :accessor component-id)
   (children
    :initarg :children
    :initform nil
    :accessor component-children)))

(defmethod print-object ((component component) stream)
  (print-unreadable-object (component stream :type t)
    (format stream "#~D" (component-id component))))

(defmethod initialize-instance :after ((component component) &key)
  (let ((id (parse-integer (subseq (symbol-name (gensym "G")) 1))))
    (setf (component-id component) id)))

(defun append-child (parent child)
  (appendf (component-children parent) (list child)))

;; attributes => plist
;; body => list
(defun segment-attributes-children (form)
  (let* ((body (loop for rest on form by #'cddr
                  unless (and (keywordp (car rest)) (cdr rest))
                  return rest))
         (attributes (ldiff form body)))
    (values attributes body)))

(defmacro define-component (name direct-superclasses direct-slots)
  `(progn
     (defclass ,name (,@direct-superclasses component)
       ,direct-slots)
     (defun ,name (&rest attributes-and-children)
       (multiple-value-bind (attributes children)
           (segment-attributes-children attributes-and-children)
         (let ((component (apply 'make-instance ',name attributes)))
           (loop for child in children
              do (append-child component child))
           component)))))
