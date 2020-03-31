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
                                            properties)))
                           (restart-case
                               (signal 'on-css-rule :rule rule)
                             (continue ())))))))
         ,@body))))

(defmacro define-component-class-style-method (component-name &body body)
  `(defmethod component-class-style ((class (eql ',component-name)))
     (with-css-as-signal
       ,@body)))

(defmethod component-class-style :around (component-name)
  (let ((package (symbol-package component-name))
        (properties '())
        (rules '()))
    (handler-bind
        ((on-css-property
          (lambda (c)
            (push (slot-value c 'property) properties)
            (continue)))
         (on-css-rule
          (lambda (c)
            (push (slot-value c 'rule) rules)
            (continue))))
      (call-next-method))
    (setf rules (reverse rules))
    (if properties
        (let ((rule (make-instance 'css:style-rule
                                   :selector (format nil ".~(~A~)[package~~=\"~(~A~)\"]"
                                                     component-name
                                                     (package-name package))
                                   :declarations (reverse properties))))
          (push rule rules))
        rules)))
