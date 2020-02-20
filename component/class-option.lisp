(in-package :component)

(defmethod process-component-class-option-group (component-class (group-name (eql :class-option)) options)
  (let ((current-class-options (component-class-class-options component-class)))
    (let ((current-class-names (mapcar #'car current-class-options)))
      (delete-if
       (lambda (slot)
         (member (closer-mop:slot-definition-name slot)
                 current-class-names))
       (closer-mop:class-slots component-class))
      (setf (component-class-class-options component-class) nil)))
  (call-next-method))

(defmethod process-component-class-option (component-class (option-name (eql :class-option)) option-value)
  (let ((class-name (car option-value)))
    (unless (symbolp class-name)
      (error "Class name should be a symbol, got ~A" class-name))
    (let ((initarg (or (eval (getf (cdr option-value) :initarg))
                       (make-keyword class-name)))
          (default (eval (getf (cdr option-value) :default))))
      (push `(,class-name
              :initarg ,initarg
              :default ,default)
            (component-class-class-options component-class))
      (add-slot-to-class component-class class-name
                         :initargs (list initarg)
                         :initform nil))))
