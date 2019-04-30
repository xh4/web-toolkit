(in-package :component)

(define-component button ()
  ())

(defmethod expand ((button button))
  `(html:button
    ))
