(in-package :html-test)

(in-suite :html-test)

(test text
  (is-true (typep (text) 'text))
  (is-true (typep (text "foo") 'text))
  (signals error (text :xxx)))
