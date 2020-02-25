(in-package :http-test)

(in-suite :http-test)

(test client/make-request

  (it "missing host in uri"
    (signals error (http::make-request "/" "GET" nil nil)))

  (it
    (let ((req (http::make-request "http://coobii.com" "GET" (header "n" "v") "text")))
      (is (equal 'text-entity (type-of req)))
      (is (equal "GET" (request-method req)))
      (is (equal "http://coobii.com" (request-uri req)))
      (is (equal "text" (entity-text req)))
      (is (equal "v" (header-field-value
                      (find-header-field
                       "n"
                       req))))
      (is-true (typep (request-body req) 'vector))
      (is-false (equal 0 (length (request-body req))))))

  (it
    (let ((req (http::make-request "http://coobii.com" "GET" (header "n" "v")
                                   (json:object "foo" "bar"))))
      (is (equal 'json-entity (type-of req)))
      (is (equal "GET" (request-method req)))
      (is (equal "http://coobii.com" (request-uri req)))
      (is (equal 'json:object (type-of (entity-json req))))
      (is (equal "v" (header-field-value
                      (find-header-field
                       "n"
                       req))))
      (is-true (typep (request-body req) 'vector))
      (is-false (equal 0 (length (request-body req))))))

  (it
    (let ((req (http::make-request "http://coobii.com" "GET" (header "n" "v")
                                   #(42 42 42 42))))
      (is (equal 'request (type-of req)))
      (is (equal "GET" (request-method req)))
      (is (equal "http://coobii.com" (request-uri req)))
      (is (equal "v" (header-field-value
                      (find-header-field
                       "n"
                       req))))
      (is-true (typep (request-body req) 'vector))
      (is-true (equal 4 (length (request-body req))))))

  (it "should fail with unhandlable content"
      (signals error (http::make-request "http://coobii.com" "GET" (header "n" "v") 42)))

  (it "should make request with empty content"
      (finishes (http::make-request "http://coobii.com" "GET" (header "n" "v") nil))))

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
        (is (equal 'html-entity (type-of res)))
        (is (equal 200 (status-code res)))
        ;; TODO: parse html
        ;; (is (equal 'html:element (entity-html res)))
        )))

  (it
    (with-simple-test-server-running
        (port (lambda ()
                (reply (json:object "Hello" "world"))))
      (let ((res (http:get (uri "http://127.0.0.1" :port port))))
        (is (equal 'json-entity (type-of res)))
        (is (equal 200 (status-code res)))
        (is (equal 'json:object (type-of (entity-json res))))))))
