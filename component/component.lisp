(in-package :component)

(defclass component ()
  ((id
    :initarg :id
    :initform nil
    :accessor id)
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
  `(progn
     (defclass ,name (,@super-components component)
       ,slots)
     (defun ,name (&rest arguments)
       (multiple-value-bind (attributes children)
           (segment-attributes-children arguments)
         (let ((component (apply 'make-instance ',name attributes)))
           (loop for child in children
              do (append-child component child))
           component)))))
