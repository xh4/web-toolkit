(in-package :css)

;; https://drafts.csswg.org/css-sizing-3

(define-property width ()
  ()
  (:value :auto length percentage :min-content :max-content))

(define-property height ()
  ()
  (:value :auto length percentage :min-content :max-content))

(define-property min-width ()
  ()
  (:value :auto length percentage :min-content :max-content))

(define-property max-height ()
  ()
  (:value :auto length percentage :min-content :max-content))
