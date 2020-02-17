(in-package :http-test)

(in-suite :http-test)

(test client
  (it
    (http::with-connection ("http://example.com")
      (http::get "/")
      (http::receive (res)
        (is (equal 200 (status-code res)))
        (let ((body (http::read-response-body-into-string res)))
          (is-true (stringp body))
          (is-true (> (length body) 0)))))))

(test client/https
  (it
    (http::with-connection ("https://example.com")
      (http::get "/")
      (http::receive (res)
        (is (equal 200 (status-code res)))
        (let ((body (http::read-response-body-into-string res)))
          (is-true (stringp body))
          (is-true (> (length body) 0)))))))
