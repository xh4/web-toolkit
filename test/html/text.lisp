(in-package :html-test)

(in-suite :html-test)

(test text
  (signals error (text))
  (is (typep (text "foo") 'text)))
