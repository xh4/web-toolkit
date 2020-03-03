(in-package :style)

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
    :accessor declaration-important)))

(defmethod print-object ((declaration declaration) stream)
  (print-unreadable-object (declaration stream :type t)
    (format stream "~A" (declaration-value declaration))))

(defmacro define-declaration (name superclasses slots &rest options)
  (unless (find 'declaration superclasses)
    (appendf superclasses '(declaration)))
  (let ((declaration-name (make-keyword (symbol-name name))))
    `(progn
       (defclass ,name ,superclasses ,slots)
       (defmacro ,name (value)
         `(make-instance ',',name
                         :name ,,declaration-name
                         :value ,value)))))

(defclass property (declaration) ())

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

(defclass descriptor (declaration) ())

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
