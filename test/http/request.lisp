(in-package :http-test)

(in-suite :http-test)

(test read-request-line
  (with-input-from-lines (stream '("GET / HTTP/1.1"))
    (is (equal '("GET" "/" "HTTP/1.1") (http::read-request-line stream))))

  (with-input-from-lines (stream '("GET /"))
    (is (equal nil (http::read-request-line stream))))

  (with-input-from-lines (stream '("GET"))
    (is (equal nil (http::read-request-line stream)))))
