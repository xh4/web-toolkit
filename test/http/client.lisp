(in-package :http-test)

(in-suite :http-test)

(test client
  (it
    (let ((res (request "http://example.com")))
      (is (equal 200 (status-code res)))
      (let ((body (http::read-response-body-into-string res)))
        (is-true (stringp body))
        (is-true (> (length body) 0))))))

(test client/https
  (it
    (let ((res (request "https://example.com")))
      (is (equal 200 (status-code res)))
      (let ((body (http::read-response-body-into-string res)))
        (is-true (stringp body))
        (is-true (> (length body) 0))))))
