(in-package :css)

;; https://drafts.csswg.org/css-logical

(define-property float ()
  ()
  (:value :left :right :none :inherit))

(define-property clear ()
  ()
  (:value :none :left :right :both :inherit))

;; TODO: caption-side
(define-property caption-side () ())

(define-property resize ()
  ()
  (:value .resize))

(define-parser .resize ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.oneof (.s "block") (.s "inline"))
               input)
      (if match-p
          (values rest (make-keyword (string-upcase value)) t)
          (values input nil nil)))))
