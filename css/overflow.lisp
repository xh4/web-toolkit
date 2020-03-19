(in-package :css)

;; https://drafts.csswg.org/css-overflow-3

(define-property overflow-x ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(define-property overflow-y ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(define-property overflow ()
  ()
  (:value .overflow))

(define-parser .overflow ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.or (.k :visible) (.k :hidden) (.k :clip) (.k :scroll) (.k :auto))
                          (.some (.whitespace))
                          (.or (.k :visible) (.k :hidden) (.k :clip) (.k :scroll) (.k :auto)))
                    (.or (.k :visible) (.k :hidden) (.k :clip) (.k :scroll) (.k :auto)))
               input)
      (if match-p
          (values rest (remove-if-not #'keywordp (flatten value)) t)
          (values input nil nil)))))

(define-property text-overflow ()
  ()
  (:value :clip :ellipsis))
