(in-package :form)

(define-component label ()
  ((for
    :initarg :for
    :initform nil
    :accessor label-for)
   (text
    :initarg :text
    :initform nil
    :accessor label-text)))

(define-render label (for text)
  (html:label :for for
   (typecase text
     (string text)
     (t (format nil "~A" text)))))
