(in-package :css)

(defclass rule ()
  ((prelude
    :initarg :prelude
    :initform nil
    :accessor rule-prelude)
   (block
    :initarg :block
    :initform nil
    :accessor rule-block)))

(defclass qualified-rule (rule)
  ((selectors
    :initarg :selectors
    :initform nil
    :accessor rule-selectors)
   (declarations
    :initarg :declarations
    :initform nil
    :accessor rule-declarations)))

(defmethod rule-prelude ((rule qualified-rule))
  (rule-selectors rule))

(defmethod rule-block ((rule qualified-rule))
  (rule-declarations rule))

(defclass at-rule (rule)
  ((name
    :initarg :name
    :initform nil
    :accessor rule-name)))

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
