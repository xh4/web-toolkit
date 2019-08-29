(in-package :form)

(define-component input (control)
  ((readonly
    :initarg :readonly
    :initform nil
    :accessor input-readonly)
   (required
    :initarg :required
    :initform nil
    :accessor input-required)
   (disabled
    :initarg :disabled
    :initform nil
    :accessor input-disabled)
   (placeholder
    :initarg :placeholder
    :initform nil
    :accessor input-placeholder)
   (autocomplete
    :initarg :autocomplete
    :initform nil
    :accessor input-autocomplete)
   (autofocus
    :initarg :autofocus
    :initform nil
    :accessor input-autofocus)))

(define-render input (name id readonly required disabled
                           placeholder value autocomplete autofocus)
  (typecase name
    (symbol (setf name (string-downcase (symbol-name name)))))
  (html:input :name name
              :id id
              :readonly readonly
              :required required
              :disabled disabled
              :placeholder placeholder
              :value value
              :autocomplete autocomplete
              :autofocus autofocus))


(define-component text-input (input)
  ())

(define-render text-input ()
  (let ((input (call-next-method)))
    (dom:set-attribute input "type" "text")
    input))


(define-component email-input (input)
  ())

(define-render email-input ()
  (let ((input (call-next-method)))
    (dom:set-attribute input "type" "email")
    input))


(define-component password-input (input)
  ())

(define-render password-input ()
  (let ((input (call-next-method)))
    (dom:set-attribute input "type" "password")
    input))
