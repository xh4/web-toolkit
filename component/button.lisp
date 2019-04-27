(in-package :component)

(define-component button ()
  ())

(defmethod expand ((button button))
  `(button
    ,@(component-children button)))
