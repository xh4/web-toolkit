(in-package :component)

(define-component text ()
  ((content
    :initarg :content
    :initform ""
    :accessor text-content)))

;; Override the default constructor
(defun text (content)
  (make-instance 'text :content content))

(defmethod expand ((text text))
  `(html:text
    ,(text-content text)))
