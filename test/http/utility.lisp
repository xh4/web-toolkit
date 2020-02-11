(in-package :http-test)

(in-suite :http-test)

(test read-line
  (it
    (with-input-from-lines (stream '("foo"))
      (is (equal "foo" (http::read-line stream)))
      (is-true (stream-empty-p stream))))

  (it
    (with-input-from-lines (stream '("foo" "bar"))
      (is (equal "foo" (http::read-line stream)))
      (is-true (stream-length-p 5 stream))))

  (it
    (with-input-from-lines (stream '("foo: bar"))
      (is (equal "foo: bar" (http::read-line stream)))
      (is-true (stream-empty-p stream))))

  (it
    (with-input-from-lines (stream '(""))
      (is (equal "" (http::read-line stream)))))

  (it
    (with-input-from-lines (stream '("" "foo"))
      (is (equal "" (http::read-line stream)))
      (is-true (stream-length-p 5 stream))))

  (it
    (with-input-from-lines (stream '("foo" "bar") :line-break #\Return)
      (is (null (http::read-line stream)) "Wrong line break #\\Return")))

  (it
    (with-input-from-lines (stream '("foo" "bar") :line-break #\Newline)
      (is (null (http::read-line stream)) "Wrong line break #\\Newline")))

  (it
    (with-input-from-lines (stream '("你好"))
      (is (null (http::read-line stream))))))
