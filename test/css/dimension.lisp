(in-package :css-test)

(in-suite :css-test)

(test dimension-unit
  (is (equal 'px (type-of (px 11))))

  (is (equal 11 (css::dimension-number (px 11))))
  (is (equal :px (css::dimension-unit (px 11))))

  (signals error (px nil))

  (signals error (px "11")))

(test dimension
  (it
    (is (equal nil (css::dimension 11)))

    (is (equal nil (css::dimension "11")))

    (is (equal nil (css::dimension nil)))

    (let ((d (css::dimension "11px")))
      (is (equal 'px (type-of d))))

    (let ((d (css::dimension "11px" 'css::length)))
      (is (equal 'px (type-of d))))

    (is (equal nil (css::dimension "11px" 'css::angle)))

    ))
