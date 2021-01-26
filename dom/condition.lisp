(in-package :dom)

(define-condition exception () ())

(defmacro define-exception (name))

(define-exception invalid-character-error)
