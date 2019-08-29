(in-package :form)

(define-component textual-field (field)
  ((readonly
    :initarg :readonly
    :initform nil
    :accessor field-readonly)
   (required
    :initarg :required
    :initform nil
    :accessor field-required)
   (disabled
    :initarg :disabled
    :initform nil
    :accessor field-disabled)
   (placeholder
    :initarg :placeholder
    :initform nil
    :accessor field-placeholder)
   (autocomplete
    :initarg :autocomplete
    :initform nil
    :accessor field-autocomplete)
   (autofocus
    :initarg :autofocus
    :initform nil
    :accessor field-autofocus)))

(defmethod field-control-class ((field textual-field)) 'text-input)

(defmethod field-control-initialize-argument-names ((field textual-field))
  '(:name :value :readonly :required :disabled
    :placeholder :value :autocomplete :autofocus))

(define-component text-field (textual-field)
  ())

(defmethod field-control-class ((field text-field)) 'text-input)

(define-component email-field (textual-field)
  ())

(defmethod field-control-class ((field email-field)) 'email-input)

(define-component password-field (textual-field)
  ())

(defmethod field-control-class ((field password-field)) 'password-input)
