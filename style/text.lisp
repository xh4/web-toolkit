(in-package :style)

;; https://drafts.csswg.org/css-text-3


(define-property white-space ()
  ()
  (:value :normal :pre :nowrap :pre-wrap :break-space :pre-line))

(define-property word-break ()
  ()
  (:value :normal :keep-all :break-all :break-word))

(define-property line-break ()
  ()
  (:value :auto :loose :normal :strict :anywhere))

(define-property word-wrap ()
  ()
  (:value :normal :break-word :anywhere))

(define-property text-align ()
  ()
  (:value :start :end :left :right :center :justify :match-parent :justify-all))
