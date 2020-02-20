(in-package :component-test)

(in-suite :component-test)

(define-component button ()
  ((size
    :initarg :size))
  (:tag-option tag)
  (:class-option class :default "btn"))

(test render

  (define-render button (tag)
    tag)

  (is (render (button :tag :span)) :span)


  (define-render button (tag)
    (tag))

  (is (type-of (render (button :tag :span))) 'html:span)


  (define-render button (tag)
    (setf tag 'html:input)
    (tag))

  (is (type-of (render (button :tag :span))) 'html:input)


  (define-render button (size)
    size)

  (is (render (button :size :large)) :large)

  (define-render button (class)
    class)

  (is (render (button)) '("btn")))
