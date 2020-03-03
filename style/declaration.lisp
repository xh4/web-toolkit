(in-package :style)

(defclass declaration-class (standard-class)
  ((value
    :initarg :value
    :initform nil
    :accessor declaration-class-value)))

(defmethod validate-superclass ((class declaration-class) (super-class standard-class))
  t)

(defclass declaration ()
  ((name
    :initarg :name
    :initform nil
    :accessor declaration-name)
   (value
    :initarg :value
    :initform nil
    :accessor declaration-value)
   (important
    :initarg :important
    :initform nil
    :accessor declaration-important))
  (:metaclass declaration-class))

(defmethod print-object ((declaration declaration) stream)
  (print-unreadable-object (declaration stream :type t)
    (format stream "~A" (declaration-value declaration))))

(defmacro define-declaration (name superclasses slots &rest options)
  (unless (find 'declaration superclasses)
    (appendf superclasses '(declaration)))
  (appendf options '((:metaclass declaration-class)))
  (let ((declaration-name (make-keyword (symbol-name name))))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options)
       (defmacro ,name (value)
         `(make-instance ',',name
                         :name ,,declaration-name
                         :value ,value)))))

(defclass property (declaration) () (:metaclass declaration-class))

(defgeneric property-name (property)
  (:method ((property property))
    (declaration-name property)))

(defgeneric property-value (property)
  (:method ((property property))
    (declaration-value property)))

(defgeneric property-important (property)
  (:method ((property property))
    (declaration-important property)))

(defmacro define-property (name superclasses slots &rest options)
  (unless (find 'property superclasses)
    (appendf superclasses '(property)))
  `(define-declaration ,name ,superclasses ,slots ,@options))

(defclass descriptor (declaration) () (:metaclass declaration-class))

(defgeneric descriptor-name (descriptor)
  (:method ((descriptor descriptor))
    (declaration-name descriptor)))

(defgeneric descriptor-value (descriptor)
  (:method ((descriptor descriptor))
    (declaration-value descriptor)))

(defgeneric descriptor-important (descriptor)
  (:method ((descriptor descriptor))
    (declaration-important descriptor)))

(defmacro define-descriptor (name superclasses slots &rest options)
  (unless (find 'descriptor superclasses)
    (appendf superclasses '(descriptor)))
  `(define-declaration ,name ,superclasses ,slots ,@options))
