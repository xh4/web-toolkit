(in-package :style-test)

(in-suite :style-test)

(test dimension-unit
  (is (equal 'px (type-of (px 11))))

  (is (equal 11 (style::dimension-number (px 11))))
  (is (equal :px (style::dimension-unit (px 11))))

  (signals error (px nil))

  (signals error (px "11")))

(test dimension
  (it
    (is (equal nil (style::dimension 11)))

    (is (equal nil (style::dimension "11")))

    (is (equal nil (style::dimension nil)))

    (let ((d (style::dimension "11px")))
      (is (equal 'px (type-of d))))

    (let ((d (style::dimension "11px" 'style::length)))
      (is (equal 'px (type-of d))))

    (is (equal nil (style::dimension "11px" 'style::angle)))

    ))
