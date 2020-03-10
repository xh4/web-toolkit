(in-package :json-test)

(in-suite :json-test)

(test get
  (is (equal "bar" (json:get (object "foo" "bar") "foo")))

  (is (equal "ccc" (json:get (object "aaa"
                                     (object "bbb" "ccc"))
                             "aaa" "bbb")))

  (is (equal nil (json:get (object "aaa" "bbb") "ccc")))

  (is (equal nil (json:get (object "aaa" "bbb") "ccc" "ddd")))

  (is (equal '(1 2 3) (json:get (object "aaa" (json:array '(1 2 3))) "aaa")))

  (is (equal 2 (json:get (object "aaa" (json:array '(1 2 3))) "aaa" 1)))

  (is (equal nil (json:get (object "aaa" (json:array nil)) "aaa" 1)))

  (is (equal nil (json:get (object "aaa" (json:null)) "aaa")))

  (is (equal nil (json:get (object "aaa" nil) "aaa")))

  (is (equal 42 (json:get (object "aaa" (json:null 42)) "aaa")))

  (is (equal nil (json:get (object "aaa" (json:null nil)) "aaa"))))
