(in-package :component)

(defmethod process-component-class-option-group (component-class (group-name (eql :tag-option)) options)
  (let ((current-tag-options (component-class-tag-options component-class)))
    (let ((current-tag-names (mapcar #'car current-tag-options)))
      (delete-if
       (lambda (slot)
         (member (closer-mop:slot-definition-name slot)
                 current-tag-names))
       (closer-mop:class-slots component-class))
      (setf (component-class-tag-options component-class) nil)))
  (call-next-method))

(defmethod process-component-class-option (component-class (option-name (eql :tag-option)) option-value)
  (let ((tag-name (car option-value)))
    (unless (symbolp tag-name)
      (error "Tag name should be a symbol, got ~A" tag-name))
    (let ((initarg (or (eval (getf (cdr option-value) :initarg))
                       (make-keyword tag-name)))
          (initform (or (eval (getf (cdr option-value) :initform))
                        :div))
          (allow (eval (getf (cdr option-value) :allow))))
      (push `(,tag-name
              :initarg ,initarg
              :initform ,initform
              :allow ,allow)
            (component-class-tag-options component-class))
      (add-slot-to-class component-class tag-name
                         :initargs (list initarg)
                         :initform initform))))
