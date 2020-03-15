(in-package :css)

(defun digit-p (char)
  (char<= #\0 char #\9))

(define-parser .digit ()
  (.satisfies 'digit-p))

(defun hexdig-p (char)
  (or (digit-p char)
      (char<= #\a char #\f)
      (char<= #\A char #\F)))

(define-parser .hexdig ()
  (.satisfies 'hexdig-p))

(defun whitespace-p (char)
  (char= char #\space))

(define-parser .whitespace ()
  (.satisfies 'whitespace-p))
