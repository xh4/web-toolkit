(in-package :http-test)

(in-suite :http-test)

(test read-line
  (with-input-from-lines (stream '("foo"))
    (is (equal "foo" (http::read-line stream))))

  (with-input-from-lines (stream '("foo" "bar"))
    (is (equal "foo" (http::read-line stream))))

  (with-input-from-lines (stream '(""))
    (is (null (http::read-line stream))))

  (with-input-from-lines (stream '("foo" "bar") :line-break #\Return)
    (is (null (http::read-line stream)) "Wrong line break #\\Return"))

  (with-input-from-lines (stream '("foo" "bar") :line-break #\Newline)
    (is (null (http::read-line stream)) "Wrong line break #\\Newline"))

  (with-input-from-lines (stream '("你好"))
    (is (null (http::read-line stream)))))
