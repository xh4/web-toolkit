(in-package :css-test)

(in-suite :css-test)

(test margin
  (margin-top "1px")
  (margin-right "1px")
  (margin-bottom "1px")
  (margin-left "1px")
  (margin "1px")
  (margin "1px 2px")
  (margin "1px 2px 3px")
  (margin "1px 2px 3px 4px"))

(test padding
  (padding-top "1px")
  (padding-right "1px")
  (padding-bottom "1px")
  (padding-left "1px")
  (padding "1px")
  (padding "1px 2px")
  (padding "1px 2px 3px")
  (padding "1px 2px 3px 4px"))
