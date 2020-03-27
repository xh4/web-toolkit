(in-package :component-test)

(in-suite :component-test)

(test diff
  (is (equal nil (diff (div) (div))))

  (is (equal nil (diff (div :class "foo") (div :class "foo"))))

  (is (equal nil (diff (div :class "foo bar") (div :class "bar foo")))))

(test diff/update
  (it
    (let ((a (div :class "foo"))
          (b (div :class "bar")))
      (is (equal `((:update nil ,a (("class" . "bar")))) (diff a b)))))

  (it
    (let ((a (div :class "foo"))
          (b (div)))
      (is (equal `((:update nil ,a (("class" . nil)))) (diff a b)))))

  (it
    (let ((a (div))
          (b (div :class "bar")))
      (is (equal `((:update nil ,a (("class" . "bar")))) (diff a b))))))

(test diff/replace
  (it
    (let ((a (h1))
          (b (h2)))
      (is (equal `((:replace nil ,a ,b)) (diff a b)))))

  (it
    (let ((a (h1))
          (b (text)))
      (is (equal `((:replace (0) ,a ,b)) (diff (div a) (div b))))))

  (it
    (let ((a (text))
          (b (h1)))
      (is (equal `((:replace (0) ,a ,b)) (diff (div a) (div b))))))

  (it
    (let ((a (text "a"))
          (b (text "b")))
      (is (equal `((:replace (0) ,a ,b)) (diff (div a) (div b))))))

  (it
    (let ((a (text "xx"))
          (b (text "xx")))
      (is (equal nil (diff (div a) (div b))))))

  (it
    (is (equal nil (diff (div "xx") (div "xx"))))))

(test diff/children
  (it
    (let ((a (h1))
          (b (h2))
          (c (h1))
          (d (h2)))
      (is (equal `((:replace (0) ,a ,b)
                   (:replace (1) ,c ,d))
                 (diff (div a c) (div b d))))))

  (it
    (let* ((a (h1))
           (p-1 (div))
           (p-2 (div a)))
      (is (equal `((:insert nil ,p-1 0 ,a))
                 (diff p-1 p-2)))))

  (it
    (let* ((a (h1))
           (p-1 (div))
           (p-2 (div a a a)))
      (is (equal `((:insert nil ,p-1 0 ,a)
                   (:insert nil ,p-1 1 ,a)
                   (:insert nil ,p-1 2 ,a))
                 (diff p-1 p-2)))))

  (it
    (let* ((a (h1))
           (p-1 (div a))
           (p-2 (div)))
      (is (equal `((:remove (0) ,a))
                 (diff p-1 p-2)))))

  (it
    (let* ((a (h1))
           (p-1 (div a a))
           (p-2 (div)))
      (is (equal `((:remove (0) ,a)
                   (:remove (0) ,a))
                 (diff p-1 p-2))))))
