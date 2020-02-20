(in-package :component)

(defgeneric render (component))

(defmethod render ((element html:element)) element)

(defmethod render ((text html:text)) text)

(defmacro define-render (component-name bindings &body body)
  `(define-render-method ,component-name ,bindings ,@body))

(defmacro define-render-method (component-name bindings &body body)
  `(defmethod render ((,component-name ,component-name))
     (with-render-bindings ,component-name ,bindings
        ,@body)))

(defmacro with-render-bindings (component-name bindings &body body)
  (let ((component-class (find-class component-name))
        (binding-forms bindings)
        (function-binding-forms '())
        (variable-binding-forms '())
        (symbol-binding-forms '()))
    (loop for binding-form in binding-forms
       do
         (loop for (type . form) in (resolve-render-binding-form
                                     component-name
                                     component-class
                                     binding-form)
            do
              (case type
                (:function (appendf function-binding-forms (list form)))
                (:variable (appendf variable-binding-forms (list form)))
                (:symbol (appendf symbol-binding-forms (list form))))))
    `(let ,variable-binding-forms
       (flet ,function-binding-forms
         (symbol-macrolet ,symbol-binding-forms
           ,@body)))))

(defun resolve-render-binding-form (component-variable component-class binding-form)
  (let ((symbol binding-form))
    (cond
      ;; tag options
      ((find symbol (component-class-tag-options component-class)
             :key 'first)
       (let ((default-tag (getf
                           (cdr
                            (find symbol (component-class-tag-options
                                          component-class)
                                  :key 'first))
                           :initform)))
         `((:variable ,symbol (or
                               (slot-value ,component-variable ',symbol)
                               ,default-tag
                               :div))
           (:function ,symbol (&rest args)
                      (apply (ensure-symbol (symbol-name ,symbol)
                                            :html)
                             args)))))
      ;; class options
      ((find symbol (component-class-class-options component-class)
             :key 'first)
       (let ((default-class (ensure-list
                             (getf
                              (cdr
                               (find symbol (component-class-class-options
                                             component-class)
                                     :key 'first))
                              :default))))
         `((:variable ,symbol (append
                               ',default-class
                               (ensure-list
                                (slot-value ,component-variable ',symbol)))))))
      ((equal "ID" (symbol-name symbol))
       `((:variable ,symbol (slot-value ,component-variable '%id))))
      ((equal "CHILDREN" (symbol-name symbol))
       `((:variable ,symbol (slot-value ,component-variable '%children))
         (:symbol ,(ensure-symbol "@CHILDREN")
                  (mapcar 'render ,symbol))))
      ;; slot
      (t `((:variable ,symbol (slot-value ,component-variable ',symbol)))))))
