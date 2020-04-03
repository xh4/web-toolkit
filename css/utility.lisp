(in-package :css)

(defun take (n list)
  "Returns the first n elements of the list."
  (when (not (zerop n))
    (cons (first list) (take (1- n) (rest list)))))
