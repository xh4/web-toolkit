(in-package :css)

;; https://drafts.csswg.org/css-fonts-3

;; TODO: font-family

(define-property font-weight ()
  ()
  (:value :normal :bold :bolder :lighter
          100 200 300 400 500 600 700 800 900))

(define-property font-stretch ()
  ()
  (:value :normal :ultra-condensed :extra-condensed
          :condensed :semi-condensed :semi-expanded
          :expanded :extra-expanded :ultra-expanded))

(define-property font-style ()
  ()
  (:value :normal :italic :oblique))

(define-property font-size ()
  ()
  ;; TODO: https://drafts.csswg.org/css-fonts-3/#propdef-font-size
  (:value :xx-small :x-small :small :medium :large :x-large :xx-large
          :larger :smaller
          length percentage))

;; TODO: font shorthand
