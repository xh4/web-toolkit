(in-package :css-test)

(in-suite :css-test)

(test margin

  (let ((p (margin-top "11px")))
    (is (equal 'margin-top (type-of p)))
    (is (equal 'px (type-of (css::property-value p)))))

  (let ((p (margin-top (px 11))))
    (is (equal 'px (type-of (css::property-value p)))))

  (signals error (margin-top 11))

  (signals error (margin-top "11deg"))

  (signals error (margin-top "11"))

  (let ((p (margin-top :auto)))
    (is (equal :auto (css::property-value p))))

  (let ((p (margin-top "auto")))
    (is (equal :auto (css::property-value p))))

  (signals error (margin-top "xxx"))

  (signals error (margin-top :xxx))

  (let ((p (margin-top (% 11))))
    (is (equal 'css::percentage (type-of (css::property-value p)))))

  (let ((p (margin-top "11%")))
    (is (equal 'css::percentage (type-of (css::property-value p))))))
