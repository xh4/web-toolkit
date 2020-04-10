(in-package :ant-design)

(defmacro define-antd-component (component-name superclasses slots &rest options)
  `(define-component ,component-name ,superclasses ,slots ,@options))

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

(defmacro map-classes (element &body body)
  `(progn
     ,@(loop for form in body
             collect `(when ,(first form)
                        (add-class ,element ,(second form))))))

(defmacro map-attributes (element &body body)
  `(progn
     ,@(loop for form in body
             collect `(when-let ((value ,(first form)))
                        (typecase value
                          (string (dom:set-attribute ,element ,(second form) value))
                          (symbol
                           (cond
                            ((eq value t) (dom:set-attribute ,element ,(second form) ""))
                            (t (dom:set-attribute ,element
                                                  ,(second form)
                                                  (format nil "~(~A~)" value))))))))))
