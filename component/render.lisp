(in-package :component)

(defgeneric render (component))

(defmacro define-render-method ((component class) &body body)
  (unless component (setf component 'component))
  `(defmethod render ((,component ,class))
     (without-propagation
       (with-variable-capturing (,component)
         ,@body))))

(defmethod render :around (component)
  (let ((root (call-next-method)))
    (unless (typep root '(or html:element null))
      (error "Render of ~A returned ~A which is not of type ELEMENT"
             component root))
    (prog1 root
      (without-propagation
        ;; Set tag name
        ;; (setf (slot-value component 'dom:tag-name) (dom:tag-name root))
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
        (setf (slot-value component 'root) root)))))

(defgeneric compute-component-class (component)
  (:method (component)
    (flet ((class-name-for-component-class (class)
             (let ((package (symbol-package (class-name class))))
               (list
                (format nil "~(~A~)" (class-name class))
                (format nil "~(~A~)_~(~A~)"
                        (package-name package)
                        (class-name class))))))
      (let ((classes (compute-class-precedence-list (class-of component))))
        (loop for class in classes
           until (eq class (find-class 'component))
           append (class-name-for-component-class class)
           into classes
           finally (return (reverse classes)))))))

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

(defun render-all (node)
  (let ((render-table (make-hash-table))
        (component-list '()))
    (labels ((render-1 (node)
               (typecase node
                 (html:text node)
                 (string (html:text node))
                 (component
                  (let ((root (render node)))
                    (setf (gethash node render-table) root)
                    (pushnew node component-list)
                    (loop for i from 0
                       for child in (dom:children root)
                       do (setf (nth i (dom:children root)) (render-1 child))
                       finally (return root))))
                 (html:element
                  (loop for i from 0
                     for child in (dom:children node)
                     do (setf (nth i (dom:children node)) (render-1 child))
                     finally (return node))))))
      (values (render-1 node) render-table (reverse component-list)))))
