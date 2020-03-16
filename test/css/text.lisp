(in-package :css-test)

(in-suite :css-test)

(test text-transform
  (text-transform "none")
  (text-transform "capitalize")
  (text-transform "capitalize full-width")
  (text-transform "capitalize full-width full-size-kana"))
