(in-package :style)

;; https://drafts.csswg.org/css-flexbox-1

;; TODO: flex shorthand

(define-property flex-direction ()
  ()
  (:value :row :row-reverse :column :column-reverse))

(define-property flex-wrap ()
  ()
  (:value :nowrap :wrap :wrap-reverse))

(define-property flex-flow ()
  ()
  ;; TODO: https://drafts.csswg.org/css-flexbox-1/#flex-flow-property
  (:value :row :row-reverse :column :column-reverse
          :nowrap :wrap :wrap-reverse))

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
  (:value :content length percentage :auto :inherit))

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
