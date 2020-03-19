(in-package :css)

;; https://drafts.csswg.org/css-flexbox-1

;; TODO: flex
(define-property flex ()
  ())

(define-property flex-direction ()
  ()
  (:value .flex-direction))

(define-parser .flex-direction ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.s "row") (.s "row-reverse") (.s "column") (.s "column-reverse"))
               input)
      (if match-p
          (values rest (make-keyword (string-upcase value)) t)
          (values input nil nil)))))

(define-property flex-wrap ()
  ()
  (:value .flex-wrap))

(define-parser .flex-wrap ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.s "nowrap") (.s "wrap") (.s "wrap-reverse"))
               input)
      (if match-p
          (values rest (make-keyword (string-upcase value)) t)
          (values input nil nil)))))

(define-property flex-flow ()
  ()
  (:value .flex-flow))

(define-parser .flex-flow ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.flex-direction) (.some (.whitespace)) (.flex-wrap))
                    (.seq (.flex-wrap) (.some (.whitespace)) (.flex-direction))
                    (.flex-direction)
                    (.flex-wrap))
               input)
      (if match-p
          (values rest (remove-if-not #'keywordp (flatten (ensure-list value))) t)
          (values input nil nil)))))

;; (flex-flow "row")
;; (flex-flow "column wrap")
;; (flex-flow "row-reverse wrap-reverse")

(define-property order ()
  ()
  (:value integer))

(define-property flex-grow ()
  ()
  (:value number))

(define-property flex-shrink ()
  ()
  (:value number))

(define-property flex-basis ()
  ()
  ;; TODO: https://drafts.csswg.org/css-flexbox-1/#flex-basis-property
  (:value :content .length .percentage :auto))

(define-property justify-content ()
  ()
  (:value :flex-start :flex-end :center :space-between :space-around))

(define-property align-items ()
  ()
  (:value :flex-start :flex-end :center :baseline :stretch))

(define-property align-self ()
  ()
  (:value :auto :flex-start :flex-end :center :baseline :stretch))

(define-property align-content ()
  ()
  (:value :flex-start :flex-end :center :space-between :space-around :stretch))

(define-property align-content ()
  ()
  (:value :flex-start :flex-end :center :space-between :space-around :stretch))
