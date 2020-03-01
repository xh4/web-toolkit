(in-package :cl-user)

(defpackage :html
  (:nicknames :wt.html)
  (:use :cl :alexandria)
  (:shadow :write-char :write-string)
  #+sb-package-locks
  (:lock t)
  (:export :constructor
           :construct
           :document
           :document-title
           :document-body
           :document-head
           :document-images
           :document-links
           :document-forms
           :document-scripts
           :element
           :custom-element
           :tag-name
           :children
           :append-child
           :text
           :serialize

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
           :s :samp :script :section :select :slot :small :source :span :strong :style :sub :summary :sup
           :table :tbody :td :template :textarea :tfoot :th :thead :title :tr :track
           :u :ul
           :var :video
           :wbr)
  (:import-from :dom
                :tag-name
                :children
                :append-child))
