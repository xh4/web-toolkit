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
        (parse (.seq (.some/s (.or (.digit) (.s ".") (.s "-")))
                     (.maybe (.some/s (.alpha))))
               input)
      (if match-p
          (let ((n (if (find #\. (first value))
                       (parse-float (first value) :junk-allowed t)
                       (parse-integer (first value) :junk-allowed t)))
                (u (second value)))
            (if u
                (loop for unit in '(em ex ch rem vw vh vmin vmax
                                    cm mm q in pt pc px)
                   when (string-equal u (symbol-name unit))
                   do (return (values rest (funcall unit n) t))
                   finally (return (values input nil nil)))
                (values rest (make-instance 'length :number n) t)))
          (values input nil nil)))))

;; (parse (.length) "42px")
;; (parse (.length) "1.2rem")
;; (parse (.length) "0")
;; (parse (.length) "-5px")
;; (parse (.length) "-0.25")
