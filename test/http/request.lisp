(in-package :http-test)

(in-suite :http-test)

(test read-request-line
  (with-input-from-lines (stream '("GET / HTTP/1.1"))
    (is (equal '("GET" "/" "HTTP/1.1") (http::read-request-line stream))))

  (with-input-from-lines (stream '("GET /"))
    (is (equal nil (http::read-request-line stream))))

  (with-input-from-lines (stream '("GET"))
    (is (equal nil (http::read-request-line stream)))))

(test write-request-line
  (let ((line (with-output-to-string (stream)
                (http::write-request-line stream "GET" "/" "HTTP/1.1"))))
    (is (equal line (format nil "GET / HTTP/1.1~C~C" #\Return #\Newline)))))

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
