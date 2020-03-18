(in-package :css)

;; https://drafts.csswg.org/css-overflow-3

(define-property overflow-x ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(define-property overflow-y ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(define-property overflow ()
  ()
  (:value .overflow))

(define-parser .overflow ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.seq (.or (.s "visible") (.s "hidden") (.s "clip") (.s "scroll") (.s "auto"))
                     (.some (.whitespace))
                     (.or (.s "visible") (.s "hidden") (.s "clip") (.s "scroll") (.s "auto")))
               input)
      (if match-p
          (values rest `(,(make-keyword (string-upcase (first value)))
                          ,(make-keyword (string-upcase (third value))))
                  t)
          (values input nil nil)))))

(define-property text-overflow ()
  ()
  (:value :clip :ellipsis))
