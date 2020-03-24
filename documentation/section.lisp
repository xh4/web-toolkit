(in-package :documentation)

(define-component section ()
  ((title
    :initarg :title
    :initform nil
    :accessor title))
  (:render
   (lambda (section)
     (with-slots (title)
         (html:section
          (h2 title)
          children)))))
