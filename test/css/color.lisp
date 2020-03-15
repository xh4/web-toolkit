(in-package :css-test)

(in-suite :css-test)

(test rgb
  (is (equal 'rgb (type-of (rgb 1 2 3))))

  (is (equal 1 (css::rgb-red (rgb 1 2 3))))
  (is (equal 2 (css::rgb-green (rgb 1 2 3))))
  (is (equal 3 (css::rgb-blue (rgb 1 2 3))))

  (signals error (rgb 1))
  (signals error (rgb 1 2))
  ;; (signals error (rgb 1 2 3 4))
  (signals error (rgb "1" "2" "3"))
  (signals error (rgb 1 2 256)))

(test .rgb
  (it
    (let ((rgb (nth-value 1 (parse (css::.rgb) "rgb(1,2,3)"))))
      (is (equal 'rgb (type-of rgb)))
      (is (equal 1 (css::rgb-red rgb)))
      (is (equal 2 (css::rgb-green rgb)))
      (is (equal 3 (css::rgb-blue rgb))))

    (let ((rgb (nth-value 1 (parse (css::.rgb) "rgb ( 1 , 2 , 3 )"))))
      (is (equal 'rgb (type-of rgb)))
      (is (equal 1 (css::rgb-red rgb)))
      (is (equal 2 (css::rgb-green rgb)))
      (is (equal 3 (css::rgb-blue rgb))))))

(test rgba
  (is (equal 'rgba (type-of (rgba 1 2 3 1))))

  (is (equal 1 (css::rgb-red (rgba 1 2 3 0.4))))
  (is (equal 2 (css::rgb-green (rgba 1 2 3 0.4))))
  (is (equal 3 (css::rgb-blue (rgba 1 2 3 0.4))))
  (is (equal 0.4 (css::rgba-alpha (rgba 1 2 3 0.4))))

  (signals error (rgba 1))
  (signals error (rgba 1 2))
  (signals error (rgba 1 2 3 1.1))
  (signals error (rgba "1" "2" "3" "1"))
  (signals error (rgba 1 2 256 1)))

(test .rgba
  (it
    (let ((rgba (nth-value 1 (parse (css::.rgba) "rgba(1,2,3,.4)"))))
      (is (equal 'rgba (type-of rgba)))
      (is (equal 1 (css::rgb-red rgba)))
      (is (equal 2 (css::rgb-green rgba)))
      (is (equal 3 (css::rgb-blue rgba)))
      (is (equal 0.4 (css::rgba-alpha rgba))))

    (let ((rgba (nth-value 1 (parse (css::.rgba) "rgba ( 1 , 2 , 3 , .4 )"))))
      (is (equal 'rgba (type-of rgba)))
      (is (equal 1 (css::rgb-red rgba)))
      (is (equal 2 (css::rgb-green rgba)))
      (is (equal 3 (css::rgb-blue rgba)))
      (is (equal 0.4 (css::rgba-alpha rgba))))))
