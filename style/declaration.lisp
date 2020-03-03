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
