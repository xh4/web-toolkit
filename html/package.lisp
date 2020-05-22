(in-package :cl-user)

(defpackage :html
  (:nicknames :wt.html)
  (:use :cl :alexandria)
  (:shadow :write-char :write-string :character :end-of-file :parse-error :stream)
  #+sb-package-locks
  (:lock t)
  (:export
   ;; construct
   :constructor
   :construct
   ;; document
   :document
   ;; element
   :element
   :custom-element
   ;; text
   :text
   ;; serialize
   :serialize
   ;; parse
   :parse
   ;; element
   :a :abbr :address :area :article :aside :audio
   :b :base :bdi :bdo :blockquote :body :br :button
   :canvas :caption :cite :code :col :colgroup
   :data :datalist :dd :del :details :dfn :dialog :div :dl :dt
   :em :embed
   :fieldset :figcaption :figure :footer :form
   :h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :hr :html
   :i :iframe :img :input :ins
   :kbd
   :label :legend :li :link
   :main :mark :menu :menuitem :meta :meter
   :nav :noscript
   :object :ol :optgroup :option :output
   :p :param :picture :pre :progress
   :q
   :rb :rp :rt :rtc :ruby
   :s :samp :script :section :select :slot :small :source
   :span :strong :style :sub :summary :sup
   :table :tbody :td :template :textarea :tfoot :th :thead :title :tr :track
   :u :ul
   :var :video
   :wbr
   ;; condition
   :html-error
   :parse-error
   :abrupt-closing-of-empty-comment
   :abrupt-doctype-public-identifier
   :abrupt-doctype-system-identifier
   :absence-of-digits-in-numeric-character-reference
   :cdata-in-html-content
   :character-reference-outside-unicode-range
   :control-character-in-input-stream
   :control-character-reference
   :end-tag-with-attributes
   :duplicate-attribute
   :end-tag-with-trailing-solidus
   :eof-before-tag-name
   :eof-in-cdata
   :eof-in-comment
   :eof-in-doctype
   :eof-in-script-html-comment-like-text
   :eof-in-tag
   :incorrectly-closed-comment
   :incorrectly-opened-comment
   :invalid-character-sequence-after-doctype-name
   :invalid-first-character-of-tag-name
   :missing-attribute-value
   :missing-doctype-name
   :missing-doctype-public-identifier
   :missing-doctype-system-identifier
   :missing-end-tag-name
   :missing-quote-before-doctype-public-identifier
   :missing-quote-before-doctype-system-identifier
   :missing-semicolon-after-character-reference
   :missing-whitespace-after-doctype-public-keyword
   :missing-whitespace-after-doctype-system-keyword
   :missing-whitespace-before-doctype-name
   :missing-whitespace-between-attributes
   :missing-whitespace-between-doctype-public-and-system-identifiers
   :nested-comment
   :noncharacter-character-reference
   :noncharacter-in-input-stream
   :non-void-html-element-start-tag-with-trailing-solidus
   :null-character-reference
   :surrogate-character-reference
   :surrogate-in-input-stream
   :unexpected-character-after-doctype-system-identifier
   :unexpected-character-in-attribute-name
   :unexpected-character-in-unquoted-attribute-value
   :unexpected-equals-sign-before-attribute-name
   :unexpected-null-character
   :unexpected-question-mark-instead-of-tag-name
   :unexpected-solidus-in-tag
   :unknown-named-character-reference)
  (:import-from :dom
                :tag-name
                :children
                :append-child
                :root))
