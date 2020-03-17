(in-package :cl-user)

(defpackage :css
  (:nicknames :wt.css)
  (:use :cl :alexandria)
  (:shadow :length :float :declaration :rem :time :position :shadow)
  (:export
   ;; length
   :em :ex :ch :rem
   :vw :vh :vmin :vmax
   :cm :mm :q :in :pt :pc :px
   ;; angle
   :deg :grad :rad :turn
   ;; time
   :s :ms
   ;; frequency
   :hz :khz
   ;; resolution
   :dpi :dpcm :dppx
   ;; percentage
   :%
   ;; color
   :color :opacity :rgb :rgba
   ;; box
   :margin-top :margin-right :margin-left :margin-bottom :margin :margin-trim
   :padding-top :padding-right :padding-left :padding-bottom :padding
   ;; sizing
   :width :height :min-width :min-height :max-width :max-height :box-sizing
   ;; display
   :display
   ;; position
   :position :top :right :bottom :left
   ;; logical
   :float :clear
   ;; overflow
   :overflow-x :overflow-y :overflow :text-overflow
   ;; text
   :text-transform :white-space :word-break :line-break :hyphens
   :overflow-wrap :word-wrap :text-align :text-align-all :text-align-last
   :text-justify :word-spacing :letter-spacing :text-indent :hanging-punctuation
   ;; fonts
   :font-weight :font-stretch :font-style :font-size :font
   ;; background
   :background-color :background-image :background-repeat :background-attachment
   :background-position :background-clip :box-shadow :shadow
   ;; flexbox
   :flex :flex-direction :flex-wrap :flex-flow :order
   :flex-grow :flex-shrink :flex-basis
   :justify-content :align-items :align-self :align-content
   ;; rule
   :rule :qualified-rule :at-rule :rule-prelude :rule-block
   :rule-selectors :rule-declarations :rule-name
   ;; style
   :style :style-declarations :merge-style)
  (:import-from :utility
                :define-parser
                :parse
                :.seq
                :.seq/s
                :.s
                :.n
                :.m/s
                :.satisfies
                :.or
                :.any
                :.some
                :.some/s
                :.maybe
                :.end
                :.alpha
                :.digit
                :.hexdig
                :string-prefix-p
                :string-suffix-p)
  (:import-from :parse-float
                :parse-float)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :closer-mop
                :validate-superclass))
