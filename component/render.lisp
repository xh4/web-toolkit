(in-package :component)

(defgeneric render (component))

(defmethod render ((element html:element)) element)

(defmethod render ((text html:text)) text)

(defmacro define-render (name bindings &body body)
  (let ((binding-forms bindings)
        (children-binding-symbol nil)
        (children-binding-present-p nil)
        (component-default-tag (component-class-default-tag (find-class name))))
    (setf bindings nil)
    (loop for binding-form in binding-forms
       for variable-name = (ensure-symbol (symbol-name binding-form))
       do
         (when (equal "CHILDREN" (symbol-name variable-name))
           (setf children-binding-present-p t
                 children-binding-symbol variable-name))
         (if (member (symbol-name variable-name) '("TAG" "CLASS") :test 'equal)
             (appendf bindings `((,variable-name (slot-value ,name ',(intern (symbol-name variable-name) :component)))))
             (appendf bindings `((,variable-name (slot-value ,name ',variable-name))))))
    `(defmethod render ((,name ,name))
       (flet ((,(ensure-symbol "TAG") (&rest args)
                (apply ',(ensure-symbol (symbol-name component-default-tag)
                                        :html)
                       args))
              )
         (let ,bindings
           ,@(if children-binding-present-p
                 `((symbol-macrolet ((,(ensure-symbol "@CHILDREN")
                                      (mapcar 'render ,children-binding-symbol)))
                     ,@body))
                 body))))))
