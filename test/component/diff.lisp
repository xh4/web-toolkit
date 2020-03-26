(in-package :component-test)

(in-suite :component-test)

(test diff
  (is (equal nil (com::diff (html:div) (html:div))))

  (is (equal nil (com::diff (html:div :class "foo") (html:div :class "foo"))))

  (is (equal nil (com::diff (html:div :class "foo bar") (html:div :class "bar foo")))))

(test diff/update
  (it
    (let ((a (html:div :class "foo"))
          (b (html:div :class "bar")))
      (is (equal `((:update nil ,a (("class" . "bar")))) (com::diff a b)))))

  (it
    (let ((a (html:div :class "foo"))
          (b (html:div)))
      (is (equal `((:update nil ,a (("class" . nil)))) (com::diff a b)))))

  (it
    (let ((a (html:div))
          (b (html:div :class "bar")))
      (is (equal `((:update nil ,a (("class" . "bar")))) (com::diff a b))))))

(test diff/replace
  (it
    (let ((a (html:h1))
          (b (html:h2)))
      (is (equal `((:replace nil ,a ,b)) (com::diff a b)))))

  (it
    (let ((a (html:h1))
          (b (html:text)))
      (is (equal `((:replace (0) ,a ,b)) (com::diff (html:div a) (html:div b))))))

  (it
    (let ((a (html:text))
          (b (html:h1)))
      (is (equal `((:replace (0) ,a ,b)) (com::diff (html:div a) (html:div b))))))

  (it
    (let ((a (html:text "a"))
          (b (html:text "b")))
      (is (equal `((:replace (0) ,a ,b)) (com::diff (html:div a) (html:div b))))))

  (it
    (let ((a (html:text "xx"))
          (b (html:text "xx")))
      (is (equal nil (com::diff (html:div a) (html:div b))))))

  (it
    (is (equal nil (com::diff (html:div "xx") (html:div "xx"))))))
