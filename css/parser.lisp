(in-package :css)

(defun digit-p (char)
  (char<= #\0 char #\9))

(define-parser .digit ()
  (.satisfies 'digit-p))

(defun whitespace-p (char)
  (char= char #\space))

(define-parser .whitespace ()
  (.satisfies 'whitespace-p))
