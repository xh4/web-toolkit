(in-package :html-test)

(in-suite :html-test)

(test text
  (is (typep (text) 'text))
  (is (typep (text "foo") 'text)))
