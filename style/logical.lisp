(in-package :style)

;; https://drafts.csswg.org/css-logical/

(define-property float ()
  ()
  (:value :left :right :none :inherit))

(define-property clear ()
  ()
  (:value :none :left :right :both :inherit))

(define-property text-align ()
  ()
  (:value :start :end :left :right :center :justify :match-parent :justify-all))
