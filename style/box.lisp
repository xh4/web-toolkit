(in-package :style)

;; https://drafts.csswg.org/css-box-3

(define-property page-relative-margin-property () ())

(define-property margin-top (page-relative-margin-property)
  ()
  (:value length percentage :auto))

(define-property margin-right (page-relative-margin-property)
  ()
  (:value length percentage :auto))

(define-property margin-bottom (page-relative-margin-property)
  ()
  (:value length percentage :auto))

(define-property margin-left (page-relative-margin-property)
  ()
  (:value length percentage :auto))

(define-property page-relative-padding-property (property) ())

(define-property padding-top (page-relative-padding-property)
  ()
  (:value length percentage :auto))

(define-property padding-right (page-relative-padding-property)
  ()
  (:value length percentage :auto))

(define-property padding-bottom (page-relative-padding-property)
  ()
  (:value length percentage :auto))

(define-property padding-left (page-relative-padding-property)
  ()
  (:value length percentage :auto))