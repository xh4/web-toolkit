(in-package :css-test)

(in-suite :css-test)

(test dimension-unit
  (is (equal 'px (type-of (px 11))))

  (is (equal 11 (css::dimension-number (px 11))))
  (is (equal :px (css::dimension-unit (px 11))))

  (signals error (px nil))

  (signals error (px "11")))
