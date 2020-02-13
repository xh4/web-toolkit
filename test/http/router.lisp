(in-package :http-test)

(in-suite :http-test)

(test router
  (it "should make router"
      (let ((router (router)))
        (is (equal 'router (type-of router)))))

  (it "should make router"
      (let ((router (router
                     (:get "/" xxx-handler))))
        (is (equal 'router (type-of router)))
        (is (equal 1 (length (http::router-routes router))))))

  (it "should make router"
      (let ((router (router
                     (:get "/" foo-handler)
                     (:post "/" bar-handler))))
        (is (equal 'router (type-of router)))
        (is (equal 2 (length (http::router-routes router))))))

  (it "should failed to make router"
      (signals error (eval '(router 42))))

  (it "should failed to make router"
      (signals error (eval '(router (:love "/")))))

  (it "should failed to make router"
      (signals error (eval '(router ("GET" "/"))))))

(test simple-route
  (it
    (define-handler foo () () (:function (lambda () (reply "foo"))))
    (let* ((router (router
                    (:get "/" foo)))
           (route (first (http::router-routes router))))
      (is (equal :get (http::route-method route)))
      (is (equal "/" (http::route-path route)))
      (is-true (functionp (http::route-matcher route)))
      (is-true (funcall (http::route-matcher route)
                        (make-instance 'request
                                       :method "GET"
                                       :uri "/")))
      (is-false (funcall (http::route-matcher route)
                        (make-instance 'request
                                       :method "GET"
                                       :uri "/foo")))
      (is-false (funcall (http::route-matcher route)
                         (make-instance 'request
                                        :method "POST"
                                        :uri "/"))))))

(test route
  (it
    (define-handler foo () () (:function (lambda () (reply "foo"))))
    (define-handler bar () () (:function (lambda () (reply "bar"))))
    (let ((router (router
                   (:get "/foo" foo)
                   (:get "/bar" bar))))
      (let ((res (http::invoke-handler router (make-instance 'request
                                                             :method "GET"
                                                             :uri "/foo"))))
        (is (equal "foo" (http::entity-body res))))
      (let ((res (http::invoke-handler router (make-instance 'request
                                                             :method "GET"
                                                             :uri "/bar"))))
        (is (equal "bar" (http::entity-body res)))))))

(test route-to-absent-handler
  (let ((router (router (:get "/" xyz-handler))))
    (let ((res (http::invoke-handler router (make-instance 'request
                                                           :method "GET"
                                                           :uri "/"))))
      (is (equal 500 (status-code res))))))
