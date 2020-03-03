(in-package :style)

;; https://drafts.csswg.org/css-display

(define-property display ()
  ()
  ;; TODO: https://drafts.csswg.org/css-display-3/#propdef-display
  (:value
   ;; display-outside
   :block :inline :run-in
   ;; display-inside
   :flow :flow-root :table :flex :grid :ruby
   ;; display-box
   :contents :none
   ;;display-legacy
   :inline-block :inline-table :inline-flex :inline-grid))
