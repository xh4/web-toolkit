(in-package :html)

(defclass text (dom:text) ())

(defclass text-constructor (constructor) ())

(defparameter text (make-instance 'text-constructor))

(defun text (&optional data)
  (check-type data (or null string))
  (construct text data))

(defmethod construct ((constructor text-constructor) &rest arguments)
  (let ((data (first arguments)))
    (make-instance 'text
                   :data data)))
