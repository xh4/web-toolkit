(in-package :css)

(define-dimension length ())

(define-dimension relative-length (length))

(define-dimension font-relative-length (relative-length))

(define-dimension-unit em (font-relative-length))

(define-dimension-unit ex (font-relative-length))

(define-dimension-unit ch (font-relative-length))

(define-dimension-unit rem (font-relative-length))

(define-dimension viewport-percentage-length (relative-length))

(define-dimension-unit vw (viewport-percentage-length))

(define-dimension-unit vh (viewport-percentage-length))

(define-dimension-unit vmin (viewport-percentage-length))

(define-dimension-unit vmax (viewport-percentage-length))

(define-dimension absolute-length (length))

(define-dimension-unit cm (absolute-length))

(define-dimension-unit mm (absolute-length))

(define-dimension-unit q (absolute-length))

(define-dimension-unit in (absolute-length))

(define-dimension-unit pt (absolute-length))

(define-dimension-unit pc (absolute-length))

(define-dimension-unit px (absolute-length))

(define-parser .length ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.seq (.some/s (.digit))
                     (.some/s (.alpha)))
               input)
      (if match-p
          (let ((n (parse-integer (first value)))
                (u (second value)))
            (loop for unit in '(em ex ch rem vw vh vmin vmax
                                cm mm q in pt pc px)
               when (string-equal u (symbol-name unit))
               do (return (values rest (funcall unit n) t))
               finally (return (values input nil nil))))
          (values input nil nil)))))

(defun parse-length (string)
  (nth-value 1 (parse (.length) string)))
