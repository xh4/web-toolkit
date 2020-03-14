(in-package :json-test)

(in-suite :json-test)

(test get
  (it
    (multiple-value-bind (value present-p)
        (json:get (object "foo" "bar") "foo")
      (is (equal "bar" value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa"
                          (object "bbb" "ccc"))
                  "aaa" "bbb")
      (is (equal "ccc" value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" "bbb") "ccc")
      (is (equal nil value))
      (is-false present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" "bbb") "ccc" "ddd")
      (is (equal nil value))
      (is-false present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array '(1 2 3))) "aaa")
      (is (equal '(1 2 3) value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array nil)) "aaa")
      (is (equal nil value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array '(1 2 3))) "aaa" 1)
      (is (equal 2 value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array '(1 2 3))) "aaa" 3)
      (is (equal nil value))
      (is-false present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array nil)) "aaa" 1)
      (is (equal nil value))
      (is-false present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:null)) "aaa")
      (is (equal nil value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" nil) "aaa")
      (is (equal nil value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (json:array '(1 2 3)) 0)
      (is (equal 1 value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (json:array '(1 2 3)) 3)
      (is (equal nil value))
      (is-false present-p))))
