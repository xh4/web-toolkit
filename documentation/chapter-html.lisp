(in-package :documentation)

(defvar *element-constructors*
  '((:a :abbr :address :area :article :aside :audio)
    (:b :base :bdi :bdo :blockquote :body :br :button)
    (:canvas :caption :cite :code :col :colgroup)
    (:data :datalist :dd :del :details :dfn :dialog :div :dl :dt)
    (:em :embed)
    (:fieldset :figcaption :figure :footer :form)
    (:h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :hr :html)
    (:i :iframe :img :input :ins)
    (:kbd)
    (:label :legend :li :link)
    (:main :mark :menu :menuitem :meta :meter)
    (:nav :noscript)
    (:object :ol :optgroup :option :output)
    (:p :param :picture :pre :progress)
    (:q)
    (:rb :rp :rt :rtc :ruby)
    (:s :samp :script :section :select :slot :small :source)
    (:span :strong :style :sub :summary :sup)
    (:table :tbody :td :template :textarea :tfoot :th :thead :title :tr :track)
    (:u :ul)
    (:var :video)
    (:wbr)))

(define-variable chapter-html
    (chapter
     :title "HTML"
     (p "The WT.HTML system implements HTML constructor, parser and serializer based on recent version of " (a :href "https://html.spec.whatwg.org/multipage/" "HTML Living Standard") ". It utilizes the Document Object Model (DOM) provided by WT.DOM.")
     (class/o :symbol 'html:document
              :summary (p "A class which represents a " (a :href "https://html.spec.whatwg.org/multipage/dom.html#documents" "HTML Document") "."))
     (function/o :symbol 'document
                 :syntax `(document &optional child)
                 :arguments `((child "An " ,(class-ref 'html:element) "."))
                 :values `((document "A " ,(class-ref 'html:document) " node."))
                 :summary (list
                           (p "The function to construct a " (class-ref 'html:document) " node.")
                           (evil (document))
                           (evil (document (html (head) (body))))))
     (class/o :symbol 'html:element
              :summary (p "A class which represents a " (a :href "https://html.spec.whatwg.org/multipage/dom.html#elements" "HTML Element") "."))
     (article
      :title "Element Constructors"
      (p "An element constructor is a function to construct a HTML element. It accepts attributes and children as arguments.")
      (evil (h1 "The title"))
      (evil (img :src "https://www.wikipedia.org/portal/wikipedia.org/assets/img/Wikipedia-logo-v2.png"))
      (evil (html:section
             (h1) (h1 (h2 (h3))) (h1)))
      (p "The " (span :class "package-name" "html") " package exposes the following element constructors: "
         (loop for symbol in (flatten *element-constructors*)
            collect (html:code (string-downcase (symbol-name symbol)))
            collect " ")))
     (class/o :symbol 'html:text
              :summary (p "A class which represents a " (a :href "https://html.spec.whatwg.org/multipage/syntax.html#text-2" "HTML Text") "."))
     (function/o :symbol 'text
                 :syntax '(text &optional data)
                 :arguments `((data "A string as the data of the text node."))
                 :values `((text "A " ,(class-ref 'html:text) " node."))
                 :summary (list
                           (p "The function to construct a " (class-ref 'html:text) " node.")
                           (evil (text))
                           (evil (text "Hello, world"))
                           (p "When passing string as child to element constructor, the string is automatically converted to a Text. " (html:code "(h1 \"title\")") " is the same as " (html:code "(h1 (text \"title\"))."))))
     (function/o :symbol 'serialize
                 :syntax '(serialize root &optional stream)
                 :arguments `((root "A node ("
                                    ,(class-ref 'html:document) ", "
                                    ,(class-ref 'html:element) " or "
                                    ,(class-ref 'html:text) ").")
                              (stream "A stream or NIL."))
                 :values `((output "NIL or a string, depends on the " ,(argument-name 'stream) " argument."))
                 :summary (list
                           (p "Serialize a HTML node to a stream.")
                           (evil (serialize (div (span "1") (span "2"))))))))
