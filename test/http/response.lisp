(in-package :http-test)

(in-suite :http-test)

(test read-response-line
  (it
    (with-input-from-lines (stream '("HTTP/1.1 200 OK"))
      (is (equal '("HTTP/1.1" 200 "OK") (http::read-status-line stream)))))

  (it
    (with-input-from-lines (stream '("HTTP/1.1 200"))
      (is (equal nil (http::read-status-line stream)))))

  (it
    (with-input-from-lines (stream '("HTTP/1.1"))
      (is (equal nil (http::read-status-line stream)))))

  ;; TODO: should raise error
  (it
    (with-input-from-lines (stream '("HTTP/1.1 42 OK"))
      (is (equal '("HTTP/1.1" 42 "OK") (http::read-status-line stream))))))
