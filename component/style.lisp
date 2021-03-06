(in-package :component)

(defgeneric component-class-style (component-name))

(defun css-properties ()
  (loop for class in (class-direct-subclasses (find-class 'css:property))
     for name = (class-name class)
     when (fboundp name)
     collect name))

(define-condition on-css-property ()
  ((property
    :initarg :property
    :initform nil)))

(define-condition on-css-rule ()
  ((rule
    :initarg :rule
    :initform nil)))

(defmacro with-css-as-signal (&body body)
  (let ((properties (css-properties)))
    `(flet ,(loop for property in properties
                  collect `(,property (value)
                             (let ((property (funcall (symbol-function ',property) value)))
                               (restart-case
                                   (signal 'on-css-property :property property)
                                 (continue ())))))
       (macrolet ((css:rule (selector &body body)
                    `(let ((properties '()))
                       (handler-bind
                           ((on-css-property
                             (lambda (c)
                               (push (slot-value c 'property) properties)
                               (continue))))
                         ,@body
                         (let ((rule (apply (symbol-function 'css:rule)
                                            ,selector
                                            (reverse properties))))
                           (restart-case
                               (signal 'on-css-rule :rule rule)
                             (continue ()))))))
                  (css:property (name value)
                    `(let ((property (funcall (symbol-function 'css:property) ,name ,value)))
                       (restart-case
                           (signal 'on-css-property :property property)
                         (continue ())))))
         ,@body))))

(defmacro define-component-class-style-method (component-name &body body)
  `(defmethod component-class-style ((class (eql ',component-name)))
     (with-css-as-signal
       ,@body)))

(defmethod component-class-style (component))

(defmethod component-class-style :around (component)
  (let* ((component-name (typecase component
                           (symbol component)
                           (component (class-name (class-of component)))))
         (package (symbol-package component-name))

         (rules '()))
    (handler-bind
        ((on-css-rule
          (lambda (c)
            (push (slot-value c 'rule) rules)
            (continue))))
      (call-next-method))
    (reverse rules)))
