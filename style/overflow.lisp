(in-package :style)

;; https://drafts.csswg.org/css-overflow-3

(define-property overflow-x ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(define-property overflow-y ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(define-property text-overflow ()
  ()
  (:value :clip :ellipsis))
