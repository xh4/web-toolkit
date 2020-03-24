(in-package :documentation)

(defun symbols (&rest symbols)
  (p
   (loop for symbol in symbols
      collect (html:code " " (string-downcase (symbol-name symbol)) " "))))

(define-variable chapter-css
    (chapter
     :title "CSS"
     (p "The WT.CSS implements CSS constructor, parser and serializer based on specifications introduced in " (a :href "https://www.w3.org/TR/css-2018/#css" :target "blank" "CSS Snapshot 2018") ".")
     (class/o :symbol 'css:declaration
              :summary (p "A class which represents a CSS " (a :href "https://www.w3.org/TR/css-syntax-3/#declaration" "declaration") ". Declarations are further categorized as \"" (class-ref 'css:property "properties") "\" or \"" (class-ref 'css:descriptor "descriptors") "\"."))
     (class/o :symbol 'css:property
              :summary (p "A class which represents a CSS property."))
     (class/o :symbol 'css:descriptor
              :summary (p "A class which represents a CSS descriptor."))
     (article
      :title "Property Constructors"
      (p "A property contructor is a function to constructor a CSS " (class-ref 'css:property) ".")
      (evil (css:margin "10px auto") :css)
      (evil (css:border-color "rgb(173, 255, 47)") :css)
      (p "The CSS package exports the following property constructor symbols:")
      (p (a :href "https://www.w3.org/TR/css-color-3" "Color"))
      (symbols :color :opacity)
      (p (a :href "https://drafts.csswg.org/css-box-3" "Box"))
      (symbols :margin :margin-top :margin-right :margin-left :margin-bottom :margin-trim
               :padding :padding-top :padding-right :padding-left :padding-bottom)
      (p (a :href "https://drafts.csswg.org/css-sizing-3" "Sizing"))
      (symbols :width :height :min-width :min-height :max-width :max-height :box-sizing
               :line-height :vertical-align)
      (p (a :href "https://drafts.csswg.org/css-display" "Display"))
      (symbols :position :top :right :bottom :left)
      (p (a :href "https://drafts.csswg.org/css-logical" "Logical"))
      (symbols :float :clear :resize :caption-side)
      (p (a :href "https://drafts.csswg.org/css-overflow-3" "Overflow"))
      (symbols :overflow :overflow-x :overflow-y :text-overflow)
      (p (a :href "https://drafts.csswg.org/css-text-3" "Text"))
      (symbols :text-transform :white-space :word-break :line-break :hyphens
               :overflow-wrap :word-wrap :text-align :text-align-all :text-align-last
               :text-justify :word-spacing :letter-spacing :text-indent :hanging-punctuation
               :text-decoration-line :text-decoration-style :text-decoration-color :text-decoration
               :text-decoration-position :text-emphasis-style :text-emphasis-color :text-emphasis
               :text-emphasis-position :text-shadow :text-decoration-skip-ink)
      (p (a :href "https://drafts.csswg.org/css-fonts-3" "Font"))
      (symbols :font :font-family :font-weight :font-stretch :font-style :font-size)
      (p (a :href "https://drafts.csswg.org/css-backgrounds-3" "Background"))
      (symbols :background :background-color :background-image :background-repeat :background-attachment
               :background-position :background-clip :background-origin :background-size
               :box-shadow :shadow)
      (p (a :href "https://drafts.csswg.org/css-backgrounds-3/#borders" "Border"))
      (symbols :border-color :border-top-color :border-right-color :border-bottom-color
               :border-left-color :border-style :border-top-style :border-right-style
               :border-bottom-style :border-left-style :border-width
               :border-top-width :border-right-width
               :border-bottom-width :border-left-width
               :border :border-top :border-right :border-bottom :border-left
               :border-radius
               :border-top-left-radius :border-top-right-radius
               :border-bottom-right-radius :border-bottom-left-radius
                :border-collapse)
      (p (a :href "https://drafts.csswg.org/css-flexbox-1" "Flexbox"))
      (symbols :flex :flex-direction :flex-wrap :flex-flow :order
               :flex-grow :flex-shrink :flex-basis
               :justify-content :align-items :align-self :align-content)
      (p (a :href "https://drafts.csswg.org/css-ui-3" "User Interface"))
      (symbols :outline :outline-width :outline-style :outline-color :outline-offset
               :user-select :cursor :caret :caret-color :caret-shape :nav-up :nav-right
               :nav-down :nav-left :appearance)
      (p (a :href "https://drafts.csswg.org/css-lists-3" "List"))
      (symbols :list-style-image :list-style-type :list-style-position :list-style :marker-side
               :counter-reset :counter-increment :counter-set)
      (p (a :href "https://drafts.csswg.org/css-content-3" "Content"))
      (symbols :content)
      (p (a :href "https://drafts.csswg.org/css-transitions" "Transition"))
      (symbols :transition :transition-property :transition-duration
               :transition-timing-function :transition-delay )
      (p (a :href "https://drafts.csswg.org/css-transforms-2" "Transform"))
      (symbols :transform :translate :scale :rotate :transform-style :perspective
               :perspective-origin :translate :backface-visibility)
      (p (a :href "https://drafts.csswg.org/css-animations" "Animation"))
      (symbols :animation :animation-name :animation-duration :animation-timing-function
               :animation-iteration-count :animation-direction :animation-play-state
               :animation-delay :animation-fill-mode)
      (p (a :href "https://drafts.fxtf.org/css-masking-1" "Masking"))
      (symbols :clip :clip-path :clip-rule :mask :mask-image :mask-mode :mask-repeat
               :mask-position :mask-clip :mask-origin :mask-size :mask-composite
               :mask-border-source :mask-border-mode :mask-border-slice :mask-border-width
               :mask-border-outset :mask-border-repeat :mask-border :mask-type)
      (p (a :href "https://www.w3.org/TR/pointerevents2" "Pointer Events"))
      (symbols :pointer-events :touch-action)
      (p (a :href "https://drafts.fxtf.org/filter-effects-2" "Filter Effects"))
      (symbols :backdrop-filter))))
