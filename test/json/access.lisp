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
        (json:get (object "aaa" (json:array 1 2 3)) "aaa")
      (is (equal '(1 2 3) value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array)) "aaa")
      (is (equal nil value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" json:true) "aaa")
      (is (equal t value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" json:false) "aaa")
      (is (equal nil value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" json:null) "aaa")
      (is (equal nil value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array 1 2 3)) "aaa" 1)
      (is (equal 2 value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array 1 2 3)) "aaa" 3)
      (is (equal nil value))
      (is-false present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" (json:array)) "aaa" 1)
      (is (equal nil value))
      (is-false present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (object "aaa" nil) "aaa")
      (is (equal nil value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (json:array 1 2 3) 0)
      (is (equal 1 value))
      (is-true present-p)))

  (it
    (multiple-value-bind (value present-p)
        (json:get (json:array 1 2 3) 3)
      (is (equal nil value))
      (is-false present-p))))

(test set
  (it
    (let ((thing (object "foo" "bar")))
      (is (equal "bar2" (setf (json:get thing "foo") "bar2")))
      (is (equal "bar2" (json:get thing "foo")))))

  (it
    (let ((thing (json:array 1 2 3)))
      (is (equal 42 (setf (json:get thing 0) 42)))
      (is (equal 42 (json:get thing 0)))))

  (it
    (let ((thing (object "foo" (object "goo" "gle"))))
      (is (equal "gle2" (setf (json:get thing "foo" "goo") "gle2")))
      (is (equal "gle2" (json:get thing "foo" "goo")))
      (is (equal "bii" (setf (json:get thing "foo" "coo") "bii")))
      (is (equal "bii" (json:get thing "foo" "coo")))))

  (it
    (let ((thing (json:array 1 2 3)))
      (signals error (setf (json:get thing 3) 42))))

  (it
    (let ((thing (object "foo" "bar")))
      (is (equal "gle" (setf (json:get thing "goo") "gle")))
      (is (equal "gle" (json:get thing "goo")))))

  (it
    (let ((thing (object "foo" (json:array 1 2 3))))
      (is (equal 42 (setf (json:get thing "foo" 0) 42)))
      (is (equal 42 (json:get thing "foo" 0)))))

  (it
    (let ((thing (object "foo" nil)))
      (signals error (setf (json:get thing "foo" "bar") 42))))

  (it
    (let ((thing (object "foo" nil))
          (v (json:array)))
      (is (eq v (setf (json:get thing "foo") v)))
      (is (equal nil (json:get thing "foo")))))

  (it
    (let ((thing (object "foo" nil))
          (v json:true))
      (is (eq v (setf (json:get thing "foo") v)))
      (is (equal t (json:get thing "foo")))))

  (it
    (let ((thing (object "foo" nil))
          (v json:false))
      (is (eq v (setf (json:get thing "foo") v)))
      (is (equal nil (json:get thing "foo")))))

  (it
    (let ((thing (object "foo" nil))
          (v json:null))
      (is (eq v (setf (json:get thing "foo") v)))
      (is (equal nil (json:get thing "foo")))))

  (it
    (let ((thing (object "foo" nil))
          (v #(1 2 3)))
      (setf (json:get thing "foo") v)
      (is (equal '(1 2 3) (json:get thing "foo"))))))
