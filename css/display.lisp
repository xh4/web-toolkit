(in-package :css)

;; https://drafts.csswg.org/css-display

;; TODO: display
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

;; TODO: visibility
;; https://www.w3.org/TR/SVG11/painting.html#VisibilityProperty
(define-property visibility ()
  ())

;; TODO: z-index
;; https://www.w3.org/TR/CSS2/visuren.html#z-index
(define-property z-index ()
  ())
