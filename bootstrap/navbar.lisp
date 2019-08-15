(in-package :bootstrap)

(define-component navbar ()
  ()
  (:tag-option tag :initform :nav)
  (:class-option class :default "navbar"))

(define-render navbar (tag class children)
  (tag :class class
       @children))

(define-component navbar-brand ()
  ((link
    :initarg :link
    :initform nil))
  (:tag-option tag :initform :span)
  (:class-option class :default "navbar-brand"))

(define-render navbar-brand (tag class children link)
  (when link
    (setf tag :a))
  (tag :class class
       :href link
       @children))
