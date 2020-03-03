(in-package :style-test)

(in-suite :style-test)

(test margin

  (let ((p (margin-top "11px")))
    (is (equal 'margin-top (type-of p)))
    (is (equal 'px (type-of (style::property-value p)))))

  (let ((p (margin-top (px 11))))
    (is (equal 'px (type-of (style::property-value p)))))

  (signals error (margin-top 11))

  (signals error (margin-top "11deg"))

  (signals error (margin-top "11"))

  (let ((p (margin-top :auto)))
    (is (equal :auto (style::property-value p))))

  (let ((p (margin-top "auto")))
    (is (equal :auto (style::property-value p))))

  (signals error (margin-top "xxx"))

  (signals error (margin-top :xxx))

  (let ((p (margin-top (% 11))))
    (is (equal 'style::percentage (type-of (style::property-value p)))))

  (let ((p (margin-top "11%")))
    (is (equal 'style::percentage (type-of (style::property-value p))))))
