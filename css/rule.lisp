(in-package :css)

(defclass rule () ())

(defclass qualified-rule (rule)
  ((prelude
    :initarg :prelude
    :initform nil
    :accessor rule-prelude)
   (block
    :initarg :block
    :initform nil
    :accessor rule-block)))

(defmethod print-object ((rule qualified-rule) stream)
  (print-unreadable-object (rule stream :type t :identity t)
    (format stream "PRELUDE: ~S BLOCK: ~S"
            (rule-prelude rule)
            (rule-block rule))))

(defclass style-rule (qualified-rule)
  ((selector
    :initarg :selector
    :initform nil
    :accessor rule-selector)
   (declarations
    :initarg :declarations
    :initform nil
    :accessor rule-declarations)))

(defmethod rule-prelude ((rule style-rule))
  (rule-selector rule))

(defmethod rule-block ((rule style-rule))
  (rule-declarations rule))

(defmethod print-object ((rule style-rule) stream)
  (print-unreadable-object (rule stream :type t :identity t)
    (format stream "~S {~A}" (rule-selector rule) (cl:length (rule-declarations rule)))))

(defclass at-rule (rule)
  ((name
    :initarg :name
    :initform nil
    :accessor rule-name)
   (prelude
    :initarg :prelude
    :initform nil
    :accessor rule-prelude)
   (block
    :initarg :block
    :initform nil
    :accessor rule-block)))

(define-serialize-method ((rule qualified-rule) stream)
  (let ((selector (rule-selector rule))
        (declarations (rule-declarations rule)))
    (loop for first-p = t then nil
       for selector in (ensure-list selector)
       unless first-p
       do (format stream ",~%")
       do (format stream "~A" selector))
    (format stream " {")
    (loop for declaration in declarations
       do (format stream "~%  ")
         (serialize declaration stream)
         (format stream ";"))
    (format stream "~%}")))

(define-serialize-method ((rule at-rule) stream)
  (let ((name (rule-name rule))
        (block (rule-block rule)))
    (format stream "@~A" name)))

(defun rule (selector &rest declarations)
  (let ((ds '()))
    (loop for declaration in declarations
       when (typep declaration 'declaration)
       do (push declaration ds))
    (make-instance 'style-rule
                   :selector selector
                   :declarations (reverse ds))))
