(in-package :http-test)

(in-suite :http-test)

(test read-request-line
  (it
    (with-input-from-lines (stream '("GET / HTTP/1.1"))
      (is (equal '("GET" "/" "HTTP/1.1") (http::read-request-line stream)))))

  (it
    (with-input-from-lines (stream '("GET /"))
      (is (equal nil (http::read-request-line stream)))))

  (it
    (with-input-from-lines (stream '("GET"))
      (is (equal nil (http::read-request-line stream))))))

(test write-request-line
  (it
    (let ((line (with-output-to-string (stream)
                  (http::write-request-line stream "GET" "/" "HTTP/1.1"))))
      (is (equal line (format nil "GET / HTTP/1.1~C~C" #\Return #\Newline))))))

(test write&read-request
  (it
    (with-request-in-stream (stream (make-instance 'request
                                                   :method "GET"
                                                   :uri "/"
                                                   :version "HTTP/1.0"
                                                   :header (header "a" "1" "b" "2" "c" "3")))
      (let ((request (http::read-request stream)))
        (is (typep request 'request))
        (is (equal "GET" (request-method request)))
        (is (equal "/" (request-uri request)))
        (is (equal "HTTP/1.0" (request-version request)))
        (let ((header (request-header request)))
          (is (= 3 (length (header-fields header)))))))))

(test message-body-present-p/request
  (it
    (let ((req (make-instance 'request
                              :header (header "Content-Length" "42"))))
      (is-true (http::message-body-present-p req))))

  (it
    (let ((req (make-instance 'request
                              :header (header "Transfer-Encoding" "gzip, chunked"))))
      (is-true (http::message-body-present-p req)))))

(test transfer-encoding-chunked-p/request
  (it
    (let ((req (make-instance 'request
                              :header (header "Transfer-Encoding" "chunked"))))
      (is-true (http::transfer-encoding-chunked-p req))))

  (it
    (let ((req (make-instance 'request
                              :header (header "Transfer-Encoding" "gzip, chunked"))))
      (is-true (http::transfer-encoding-chunked-p req)))))

(test read-request-form-data
  (it
    (let ((data (babel:string-to-octets "foo=bar")))
      (babel-streams:with-input-from-sequence (stream data)
        (let ((request (make-instance 'request
                                      :header (header
                                               "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"
                                               "Content-Length" (length data))
                                      :body stream)))
          (let ((form (http::read-request-form-data request :as :alist)))
            (is (equal "foo" (caar form)))
            (is (equal "bar" (cdar form)))))))))

(test read-request-urlencoded-form-data
  (it
    (let ((data (babel:string-to-octets "foo=bar")))
      (babel-streams:with-input-from-sequence (stream data)
        (let ((request (make-instance 'request
                                      :header (header "Content-Length" (length data))
                                      :body stream)))
          (let ((form (http::read-request-urlencoded-form-data request :as :alist)))
            (is (equal "foo" (caar form)))
            (is (equal "bar" (cdar form)))))))))
