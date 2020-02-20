(in-package :component)

(define-component text ()
  ((content
    :initarg :content
    :initform ""
    :accessor text-content)))

;; Override the default constructor
(defun text (content)
  (make-instance 'text :content content))

(defmethod render ((text text))
  (html:text
   (text-content text)))

(defmethod render ((string string))
  (html:text string))

(defmethod render ((thing T))
  (html:text (format nil "~A~%" thing)))
