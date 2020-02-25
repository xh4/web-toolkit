(in-package :http-test)

(in-suite :http-test)

(test client
  (it
   (let ((res (http:get "http://example.com" :entity nil)))
     (is (equal 200 (status-code res)))
     (let ((body (http::read-response-body-into-vector res)))
       (is-true (vectorp body))
       (is-true (> (length body) 0))))

   (let ((res (http:head "http://example.com")))
     (is (equal 200 (status-code res)))
     (is (equal nil (response-body res))))))

(test client/https
  (it
   (let ((res (http:get "https://example.com" :entity nil)))
     (is (equal 200 (status-code res)))
     (let ((body (http::read-response-body-into-vector res)))
       (is-true (vectorp body))
       (is-true (> (length body) 0))))))

(test request/entity
  (it
    (with-simple-test-server-running
        (port (lambda ()
                (reply (html:h1 "Hello"))))
      (let ((res (http:get (uri "http://127.0.0.1" :port port))))
        (is (equal 'html-entity (type-of res)))))))
