(in-package :css)

(defmacro define-serialize-method ((object stream) &body body)
  `(defmethod serialize (,object &optional ,stream)
     (let ((string-stream-p (null ,stream)))
       (when string-stream-p (setf ,stream (make-string-output-stream)))
       ,@body
       (when string-stream-p
         (get-output-stream-string ,stream)))))

(defgeneric serialize (object &optional stream))

(define-serialize-method ((declaration declaration) stream)
  (let ((name (declaration-name declaration))
        (value (declaration-value declaration)))
    (format stream "~(~A~): " name)
    (serialize value stream)))

(define-serialize-method ((dimension dimension) stream)
  (let ((number (dimension-number dimension))
        (unit (dimension-unit dimension)))
    (format stream "~A~(~A~)" number (symbol-name unit))))

(define-serialize-method ((rule qualified-rule) stream)
  (let ((selectors (rule-selectors rule))
        (declarations (rule-declarations rule)))
    (loop for first-p = t then nil
       for selector in selectors
       unless first-p
       do (format stream ",~%")
       do (format stream "~A" selector))
    (format stream " {")
    (loop for declaration in declarations
       do (format stream "~%")
         (serialize declaration stream)
         (format stream ";"))
    (format stream "~%}")))

(serialize (make-instance 'qualified-rule
                          :selectors '(".foo")
                          :declarations `(,(margin-top "10px")
                                           ,(margin-top "10px"))))
