(in-package :css)

(define-property position ()
  ()
  (:value :static :relative :absolute :sticky :fixed))

(define-property top ()
  ()
  (:value :auto .length .percentage))

(define-property right ()
  ()
  (:value :auto .length .percentage))

(define-property bottom ()
  ()
  (:value :auto .length .percentage))

(define-property left ()
  ()
  (:value :auto .length .percentage))
