(in-package :css)

(defun whitespace-p (char)
  (char= char #\space))

(define-parser .whitespace ()
  (.satisfies 'whitespace-p))
