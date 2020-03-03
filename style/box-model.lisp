(in-package :style)

(defclass page-relative-margin-property (property) ())

(define-property margin-top (page-relative-margin-property)
  ()
  (:value (or length percentage :auto)))

(define-property margin-right (page-relative-margin-property)
  ()
  (:value (or length percentage :auto)))

(define-property margin-bottom (page-relative-margin-property)
  ()
  (:value (or length percentage :auto)))

(define-property margin-left (page-relative-margin-property)
  ()
  (:value (or length percentage :auto)))

(defclass page-relative-padding-property (property) ())

(define-property padding-top (page-relative-padding-property)
  ()
  (:value (or length percentage :auto)))

(define-property padding-right (page-relative-padding-property)
  ()
  (:value (or length percentage :auto)))

(define-property padding-bottom (page-relative-padding-property)
  ()
  (:value (or length percentage :auto)))

(define-property padding-left (page-relative-padding-property)
  ()
  (:value (or length percentage :auto)))
