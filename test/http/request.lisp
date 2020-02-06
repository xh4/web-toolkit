(in-package :http-test)

(in-suite :http-test)

(test read-request-line
  (with-input-from-lines (stream '("GET / HTTP/1.1"))
    (is (equal '("GET" "/" "HTTP/1.1") (http::read-request-line stream))))

  (with-input-from-lines (stream '("GET /"))
    (is (equal nil (http::read-request-line stream))))

  (with-input-from-lines (stream '("GET"))
    (is (equal nil (http::read-request-line stream)))))

(test message-body-present-p/request
  (let ((req (make-instance 'request
                            :header (header "Content-Length" "42"))))
    (is-true (http::message-body-present-p req)))

  (let ((req (make-instance 'request
                            :header (header "Transfer-Encoding" "gzip, chunked"))))
    (is-true (http::message-body-present-p req))))

(test transfer-encoding-chunked-p/request
  (let ((req (make-instance 'request
                            :header (header "Transfer-Encoding" "chunked"))))
    (is-true (http::transfer-encoding-chunked-p req)))

  (let ((req (make-instance 'request
                            :header (header "Transfer-Encoding" "gzip, chunked"))))
    (is-true (http::transfer-encoding-chunked-p req))))
