(in-package :cl-user)

(defpackage :css
  (:nicknames :wt.css)
  (:use :cl :alexandria)
  (:shadow :length :float :declaration :rem :time :position :shadow :rotate
           :parse-error :function :function-name :function-value :block)
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
   ;; declaration
   :declaration :declaration-name :declaration-value
   :property :property-name :property-value
   :descriptor
   ;; color
   :color :opacity :rgb :rgba
   ;; box
   :margin-top :margin-right :margin-left :margin-bottom :margin :margin-trim
   :padding-top :padding-right :padding-left :padding-bottom :padding
   ;; sizing
   :width :height :min-width :min-height :max-width :max-height :box-sizing
   :line-height :vertical-align
   ;; display
   :display :visibility :z-index
   ;; position
   :position :top :right :bottom :left
   ;; logical
   :float :clear :resize :caption-side
   ;; overflow
   :overflow-x :overflow-y :overflow :text-overflow
   ;; text
   :text-transform :white-space :word-break :line-break :hyphens
   :overflow-wrap :word-wrap :text-align :text-align-all :text-align-last
   :text-justify :word-spacing :letter-spacing :text-indent :hanging-punctuation
   :text-decoration-line :text-decoration-style :text-decoration-color :text-decoration
   :text-decoration-position :text-emphasis-style :text-emphasis-color :text-emphasis
   :text-emphasis-position :text-shadow :text-decoration-skip-ink
   ;; fonts
   :font :font-family :font-weight :font-stretch :font-style :font-size
   ;; background
   :background :background-color :background-image :background-repeat :background-attachment
   :background-position :background-clip :background-origin :background-size
   :box-shadow :shadow
   ;; border
   :border-top-color :border-right-color :border-bottom-color :border-left-color
   :border-color :border-top-style :border-right-style :border-bottom-style
   :border-left-style :border-style :border-top-width :border-right-width
   :border-bottom-width :border-left-width :border-width
   :border-top :border-right :border-bottom :border-left :border
   :border-top-left-radius :border-top-right-radius
   :border-bottom-right-radius :border-bottom-left-radius
   :border-radius :border-collapse
   ;; flexbox
   :flex :flex-direction :flex-wrap :flex-flow :order
   :flex-grow :flex-shrink :flex-basis
   :justify-content :align-items :align-self :align-content
   ;; ui
   :outline :outline-width :outline-style :outline-color :outline-offset
   :user-select :cursor :caret :caret-color :caret-shape :nav-up :nav-right
   :nav-down :nav-left :appearance
   ;; list
   :list-style-image :list-style-type :list-style-position :list-style :marker-side
   :counter-reset :counter-increment :counter-set
   ;; content
   :content
   ;; transition
   :transition-property :transition-duration :transition-timing-function
   :transition-delay :transition
   ;; transform
   :transform :translate :scale :rotate :transform-style :perspective
   :perspective-origin :translate :backface-visibility
   ;; animation
   :animation :animation-name :animation-duration :animation-timing-function
   :animation-iteration-count :animation-direction :animation-play-state
   :animation-delay :animation-fill-mode
   ;; masking
   :clip :clip-path :clip-rule :mask :mask-image :mask-mode :mask-repeat
   :mask-position :mask-clip :mask-origin :mask-size :mask-composite
   :mask-border-source :mask-border-mode :mask-border-slice :mask-border-width
   :mask-border-outset :mask-border-repeat :mask-border :mask-type
   ;; pointer-events
   :touch-action :pointer-events
   ;; filter-effects
   :backdrop-filter
   ;; rule
   :rule :qualified-rule :style-rule :at-rule :rule-prelude :rule-block
   :rule-selector :rule-declarations :rule-name
   ;; serialize
   :serialize
   ;; parse
   :parse-list-of-rules :syntax-error)
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
  (:import-from :parse-number
                :parse-number)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :closer-mop
                :validate-superclass))
