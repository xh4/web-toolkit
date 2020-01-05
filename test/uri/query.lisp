(in-package :uri-test)

(in-suite :uri-test)

(test uri-query
  (let ((uri "http://coobii.com?foo=bar"))
    (is (equal (first (uri-query-plist uri)) "foo"))
    (is (equal (second (uri-query-plist uri)) "bar")))
  (let ((uri "http://coobii.com?foo="))
    (is (equal (second (uri-query-plist uri)) "") "empty value"))
  (let ((uri "http://coobii.com?foo"))
    (is (equal (second (uri-query-plist uri)) nil) "null value"))
  (let ((uri "http://coobii.com?foo&goo=gle"))
    (is (equal (second (uri-query-plist uri)) nil)))
  (let ((uri "http://coobii.com?foo=%E7%88%B1"))
    (is (equal (second (uri-query-plist uri)) "爱")))
  (let ((uri "http://coobii.com?%E7%88%B1=%E4%BD%A0"))
    (is (equal (second (uri-query-plist uri)) "你"))))
