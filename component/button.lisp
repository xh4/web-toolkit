(in-package :component)

(define-component button ()
  ()
  )

(defmethod render ((button button))
  (html:button))
