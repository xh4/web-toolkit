(in-package :http-test)

(in-suite :http-test)

(test client
  (it
   (let ((res (http:get "http://example.com")))
     (is (equal 200 (status-code res)))
     (let ((body (http::read-response-body-into-string res)))
       (is-true (stringp body))
       (is-true (> (length body) 0))))

   (let ((res (http:head "http://example.com")))
     (is (equal 200 (status-code res)))
     (is (equal nil (response-body res))))))

(test client/https
  (it
   (let ((res (http:get "https://example.com")))
     (is (equal 200 (status-code res)))
     (let ((body (http::read-response-body-into-string res)))
       (is-true (stringp body))
       (is-true (> (length body) 0))))))
