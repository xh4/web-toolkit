(in-package :http-test)

(in-suite :http-test)

(test define-handler-with-instalize-nil
  (it
    (let ((handler-class (define-handler foo () () (:instanize nil))))
      (is (equal 'http::handler-class (type-of handler-class))))))

(test define-handler-with-instalize-unset
  (it
    (let ((handler (define-handler foo () ())))
      (is-true (typep handler 'http::handler)))))

(test define-handler-with-instalize-nil-and-a-function
  (it
    (define-handler test-handler () () (:function (lambda (request))) (:instanize nil))
    (is-true (functionp (http::handler-function (find-class 'test-handler))))
    (is-true (functionp (http::handler-function (make-instance 'test-handler))))
    (is (equal '(request) (http::handler-function-lambda-list (find-class 'test-handler))))
    (is (equal '(request) (http::handler-function-lambda-list (make-instance 'test-handler))))))

(test define-handler-with-instalize-nil-and-a-function-then-change-the-function
  (it
    (define-handler test-handler () () (:function (lambda (request))) (:instanize nil))
    (define-handler test-handler () () (:function (lambda (handler request))) (:instanize nil))
    (is (equal '(handler request) (http::handler-function-lambda-list (find-class 'test-handler))))
    (is (equal '(handler request) (http::handler-function-lambda-list (make-instance 'test-handler))))))

(test define-handler-with-instalize-t-without-function
  (it
    (define-handler test-handler () () (:instanize t))
    (is (equal nil (http::handler-function test-handler)))
    (is (equal nil (http::handler-function-lambda-list test-handler)))))

(test define-handler-with-instalize-t-with-a-function
  (it
    (define-handler test-handler () () (:function (lambda (request))) (:instanize t))
    (is-true (functionp (http::handler-function test-handler)))
    (is (equal '(request) (http::handler-function-lambda-list test-handler)))))

(test define-handler-with-instalize-t-with-a-function-then-change-the-function
  (it
    (define-handler test-handler () () (:function (lambda (request))) (:instanize t))
    (define-handler test-handler () () (:function (lambda (handler request))) (:instanize t))
    (is-true (functionp (http::handler-function test-handler)))
    (is (equal '(handler request) (http::handler-function-lambda-list test-handler)))))

(test define-handler-with-wrong-function-arity
  (it
    (define-handler test-handler () () (:function (lambda (handler request))))
    (signals error (define-handler test-handler () () (:function (lambda (a b c)))))
    ;; Keep function & function-lambda-list untuoched
    (is-true (functionp (http::handler-function test-handler)))
    (is (equal '(handler request) (http::handler-function-lambda-list test-handler)))))

(test define-handler-with-wrong-function-type
  (it
    (define-handler test-handler () () (:function (lambda (handler request))))
    (signals error (define-handler test-handler () () (:function 42)))
    ;; Keep function & function-lambda-list untuoched
    (is-true (functionp (http::handler-function test-handler)))
    (is (equal '(handler request) (http::handler-function-lambda-list test-handler)))))

(test define-handler-with-empty-function
  (it
    (define-handler test-handler () () (:function))
    (is-true (null (http::handler-function test-handler)))
    (is-true (null (http::handler-function-lambda-list test-handler)))))

(test define-handler-with-call-cc-function
  (it
    (define-handler test-handler () () (:function (cl-cont:lambda/cc (request))))
    (is-true (equal 'cl-cont::funcallable/cc (type-of (http::handler-function test-handler))))
    (is (equal '(request) (http::handler-function-lambda-list test-handler)))))

(test check-handler-function-lambda-list
  (it
    (finishes (http::check-handler-function-lambda-list '()))
    (finishes (http::check-handler-function-lambda-list '(request)))
    (finishes (http::check-handler-function-lambda-list '(handler request)))
    (signals error (http::check-handler-function-lambda-list '(handler request xxx)))))

(test check-handler-function
      (it
       (finishes (http::check-handler-function (lambda ())))))

(test handler-form
      (it
        (is (equal 'http::anonymous-handler
                   (type-of (http::handler-form '(lambda ())))))
        (is (equal nil (http::handler-form nil)))
        (signals error (http::handler-form '(lambda (a b c))))))

(test invoke-handler
  (it "should return a response"
      (define-handler foo ()
        ()
        (:function (lambda ())))
      (let ((res (http::invoke-handler foo nil)))
        (is (equal 'response (type-of res)))))

  (it "should return a response"
      (define-handler foo ()
        ()
        (:function (lambda () (reply (status 201)))))
      (let ((res (http::invoke-handler foo nil)))
        (is (equal 'response (type-of res)))
        (is (equal 201 (status-code (response-status res)))))))

(test call-next-handler
  (it "should be able to call next handler"
      (define-handler foo ()
        ()
        (:function (lambda ()
                     (reply (status 201))
                     (call-next-handler))))

      (define-handler bar (foo)
        ()
        (:function (lambda ()
                     (reply (status 202)))))

      (let ((res (http::invoke-handler bar nil)))
        (is (equal 'response (type-of res)))
        (is (equal 202 (status-code (response-status res))))))

  (it "should be able to access next handler's response"
      (define-handler foo ()
        ()
        (:function (lambda ()
                     (let ((res (call-next-handler)))
                       (is (equal 202 (status-code (response-status res))))))))

      (define-handler bar (foo)
        ()
        (:function (lambda ()
                     (reply (status 202)))))

      (http::invoke-handler bar nil))

  (it "should be able to call next handler even if there is none"
      (define-handler foo ()
        ()
        (:function (lambda ()
                     (let ((res (call-next-handler)))
                       (is (equal 'response (type-of res)))))))

      (http::invoke-handler foo nil)))

(test abort-handler
  (it "should abort current handler"
      (define-handler foo ()
        ()
        (:function (lambda ()
                     (reply (status 201))
                     (abort-handler)
                     (reply (status 202)))))

      (let ((res (http::invoke-handler foo nil)))
        (is (equal 'response (type-of res)))
        (is (equal 201 (status-code (response-status res))))))

  (it "should abort rest handlers"
      (define-handler foo ()
        ()
        (:function (lambda ()
                     (reply (status 201))
                     (abort-handler))))

      (define-handler bar (foo)
        ()
        (:function (lambda ()
                     (reply (status 202)))))

      (let ((res (http::invoke-handler foo nil)))
        (is (equal 'response (type-of res)))
        (is (equal 201 (status-code (response-status res)))))))
