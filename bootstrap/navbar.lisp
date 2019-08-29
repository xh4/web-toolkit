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

(define-component navbar-nav ()
  ()
  (:tag-option tag :initform :ul)
  (:class-option class :default "navbar-nav"))

(define-render navbar-nav (tag class children)
  (tag :class class
       @children))

(define-component nav-item ()
  ()
  (:tag-option tag :initform :li)
  (:class-option class :default "nav-item"))

(define-render nav-item (tag class children)
  (tag :class class
       @children))

(define-component nav-link ()
  ((link
    :initarg :link
    :initform nil))
  (:tag-option tag :initform :a)
  (:class-option class :default "nav-link"))

(define-render nav-link (tag class link children)
  (tag :class class
       :href link
       @children))
