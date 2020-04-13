(in-package :documentation)

(defun symbols (&rest symbols)
  (p
   (loop for symbol in symbols
      collect (html:code " " (string-downcase (symbol-name symbol)) " "))))

(define-variable chapter-css
    (chapter
     :title "CSS"
     (p "The WT.CSS system implements CSS constructor, tokenizer, parser and serializer based on specifications introduced in " (a :href "https://www.w3.org/TR/css-2018/#css" :target "blank" "CSS Snapshot 2018") ".")
     (class/o :symbol 'css:declaration
              :summary (p "A class which represents a CSS " (a :href "https://www.w3.org/TR/css-syntax-3/#declaration" "declaration") ". Declarations are further categorized as \"" (class-ref 'css:property "properties") "\" or \"descriptors\"."))
     (class/o :symbol 'css:property
              :summary (p "A class which represents a CSS property."))
     (function/o :symbol 'css:property
                 :syntax '(property name value)
                 :arguments `((name "A string for the name of the " ,(class-ref 'css:property) ".")
                              (value "A string for the value of the " ,(class-ref 'css:property) "."))
                 :values `((property "A " ,(class-ref 'css:property) " instance."))
                 :summary (list (p "The function to construct a " (class-ref 'css:property) ".")
                                (evil (css:property "margin" "5px 10px"))))
     (article
      :title "Property Constructors"
      (p "A property contructor is a function to constructor a " (class-ref 'css:property) ".")
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
      (symbols :backdrop-filter))
     (class/o :symbol 'css:rule
              :summary (p "A class which represents a CSS rule. Rules are further categorized as " (class-ref 'css:qualified-rule "qualified rules") " or at-rules."))
     (class/o :symbol 'css:qualified-rule
              :summary (p "A class which represents a CSS " (a :href "https://www.w3.org/TR/css-syntax-3/#qualified-rule" "qualified rule") "."))
     (function/o :symbol 'css:rule-prelude
                 :syntax '(rule-prelude rule)
                 :arguments `((rule "A " ,(class-ref 'qualified-rule "qualified rule") "."))
                 :values `((prelude "A list of tokens."))
                 :summary (p "Get the prelude part of a " (class-ref 'qualified-rule "qualified rule") "."))
     (function/o :symbol 'css:rule-block
                 :syntax '(rule-block rule)
                 :arguments `((rule "A " ,(class-ref 'qualified-rule "qualified rule") "."))
                 :values `((block "A simple block or a list of tokens."))
                 :summary (p "Get the block part of a " (class-ref 'qualified-rule "qualified rule") "."))
     (class/o :symbol 'css:style-rule
              :summary (p "A subclass of " (a :href "https://www.w3.org/TR/css-syntax-3/#qualified-rule" "qualified rule") "."))
     (function/o :symbol 'css:rule-selector
                 :syntax '(rule-selector rule)
                 :arguments `((rule "A " ,(class-ref 'style-rule "style rule") "."))
                 :values `((selector "A string or a list of strings."))
                 :summary (p "Get the selector of a " (class-ref 'style-rule "style rule") "."))
     (function/o :symbol 'css:rule-declarations
                 :syntax '(rule-declarations rule)
                 :arguments `((rule "A " ,(class-ref 'style-rule "style rule") "."))
                 :values `((declarations "A list of " ,(class-ref 'css:declaration "declarations") "."))
                 :summary (p "Get the declarations of a " (class-ref 'style-rule "style rule") "."))
     (function/o :symbol 'css:rule
                 :syntax '(rule selector &rest properties)
                 :arguments `((selector "A string or a list of strings.")
                              (properties "A list of properties."))
                 :values `((rule "A " ,(class-ref 'css:style-rule "style rule") "."))
                 :summary (list
                           (p "This function constructs a " (class-ref 'css:style-rule "style rule") ".")
                           (evil
                            (css:rule '("h1" "h2" "h3")
                                      (css:color "black")
                                      (css:line-height "1.5"))
                            :css)))
     (function/o :symbol 'css:tokenize
                 :syntax '(tokenize source)
                 :arguments `((source "A string or a stream."))
                 :values `((tokens "A list of tokens."))
                 :summary (list
                           (p "This funtion runs the tokenizer on the " (argument-name 'source) ", return a list of " (value-name 'tokens) ".")
                           (evil (css:tokenize "body { background: #fff }") :css)))
     (function/o :symbol 'css:parse-rules
                 :syntax '(parse-rules source)
                 :arguments `((source "A string or a stream."))
                 :values `((rules "A list of rules."))
                 :summary (list
                           (p "This funtion runs the parser on the " (argument-name 'source) ", return a list of " (class-ref 'css:qualified-rule "qualified rules") ". The body (block) of the rule is given as a list of tokens, to further parse it as declarations, use " (function-ref 'css:parse-declarations) ".")
                           (evil (css:parse-rules "body { background: #fff }") :css)))
     (function/o :symbol 'css:parse-declarations
                 :syntax '(parse-declaration source)
                 :arguments `((source "A simple block, a string or a stream."))
                 :values `((declarations "A list of declarations."))
                 :summary (list
                           (p "This funtion runs the parser on the " (argument-name 'source) ", return a list of " (class-ref 'css:declaration "declarations") ". The value of a declaration is given as a list of tokens, to get the string value, call " (function-ref 'css:serialize-tokens) " on the tokens.")
                           (evil (let ((rule (first (css:parse-rules "body { background: #fff }"))))
                                   (css:parse-declarations (css:rule-block rule))) :css)
                           (evil (css:parse-declarations "color: black; margin: 5px") :css)))
     (function/o :symbol 'css:serialize-tokens
                 :syntax '(serialize-tokens tokens &optional stream)
                 :arguments `((tokens "A list of tokens.")
                              (stream "A stream or NIL."))
                 :values `((output "NIL or a string."))
                 :summary (list
                           (p "Serialize a list of " (argument-name 'tokens) " to a " (argument-name 'stream) ". This function is useful to unparse a rule's prelude (selector) or a declaration's value.")
                           (evil (let ((rule (first (css:parse-rules "body h1 { color: #333 }"))))
                                   (css:serialize-tokens (css:rule-prelude rule)))
                                 :css)
                           (evil (let ((declaration (first (css:parse-declarations "box-shadow: 0 0 0 0.2rem rgba(255, 193, 7, 0.5)"))))
                                   (css:serialize-tokens (css:declaration-value declaration)))
                                 :css)))))
