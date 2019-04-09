(in-package :wt.html)

;; About kinds of HTML elements
;; https://html.spec.whatwg.org/multipage/syntax.html#elements-2

(defparameter *void-elements*
  '(:area :base :br :col :embed :hr :img
    :input :link :meta :param :source :track :wbr))

(defparameter *raw-text-elements*
  '(:script :style))

(defparameter *escapable-raw-text-elements*
  '(:textarea :title))

(defparameter *preformatted-elements*
  '(:script :style :textarea :pre))
