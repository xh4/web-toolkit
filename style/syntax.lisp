(in-package :style)

(defclass property ()
  ())

(defclass declareation ()
  ((property
    :initarg property
    :initform nil
    :accessor declaration-property)
   (value
    :initarg value
    :initform nil
    :accessor declaration-value)))

(defclass selector () ())

(defclass rule ()
  ((selectors
    :initarg :selectors
    :initform nil
    :accessor rule-selectors)
   (declaration
    :initarg :declaration
    :initform nil
    :accessor rule-declaration)))
