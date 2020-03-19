(in-package :css)

;; https://drafts.csswg.org/css-display

(define-property display ()
  ()
  (:value .display))

(define-parser .display-outside ()
  (.oneof (.k :block) (.seq/i 0 (.k :inline) (.end)) (.k :run-in)))

(define-parser .display-inside ()
  (.oneof (.seq/i 0 (.k :flow) (.end))
          (.k :flow-root)
          (.seq/i 0 (.k :table) (.end))
          (.k :flex)
          (.k :grid)
          (.seq/i 0 (.k :ruby) (.end))))

(define-parser .display-listitem ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.seq (.maybe (.display-outside))
                     (.maybe (.oneof (.k :flow) (.k :flow-root)))
                     (.k :list-item))
               input)
      (if match-p
          (values rest (remove-if #'null (flatten value)) t)
          (values input nil nil)))))

(define-parser .display-internal ()
  (.oneof (.k :table-row-group)
          (.k :table-header-group)
          (.k :table-footer-group)
          (.seq/i 0 (.k :table-row) (.end))
          (.k :table-cell)
          (.k :table-column-group)
          (.seq/i 0 (.k :table-column) (.end))
          (.k :table-caption)
          (.k :ruby-base)
          (.seq/i 0 (.k :ruby-text) (.end))
          (.k :ruby-base-container)
          (.k :ruby-text-container)))

(define-parser .display-box ()
  (.oneof (.k :contents) (.k :none)))

(define-parser .display-legacy ()
  (.oneof (.k :inline-block) (.k :inline-table) (.k :inline-flex) (.k :inline-grid)))

(define-parser .display ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.oneof (.seq (.display-outside) (.some (.whitespace)) (.display-inside))
                       (.seq (.display-inside) (.some (.whitespace)) (.display-outside))
                       (.display-outside)
                       (.display-inside)
                       (.display-listitem)
                       (.display-internal)
                       (.display-box)
                       (.display-legacy))
               input)
      (if match-p
          (values rest (remove-if-not #'keywordp (flatten (ensure-list value))) t)
          (values input nil nil)))))

;; (display "flex")
;; (display "inline-block")
;; (display "block flow")
;; (display "block flow-root")
;; (display "inline flow")
;; (display "block flex")
;; (display "inline table")

;; TODO: visibility
;; https://www.w3.org/TR/SVG11/painting.html#VisibilityProperty
(define-property visibility ()
  ())

;; TODO: z-index
;; https://www.w3.org/TR/CSS2/visuren.html#z-index
(define-property z-index ()
  ())
