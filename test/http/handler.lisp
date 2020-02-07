(in-package :http-test)

(in-suite :http-test)

(test define-handler
  (let ((handler (define-handler foo () ())))
    (is (typep handler 'http::handler)))

  (let ((handler-class (define-handler foo () () (:instanize nil))))
    (is (typep handler-class 'http::handler-class))))

(test handler-function
  (define-handler bar () () (:function (lambda (request))) (:instanize nil))
  (is-true (functionp (http::handler-function (find-class 'bar))))
  (is-true (functionp (http::handler-function (make-instance 'bar))))
  (is (equal '(request) (http::handler-function-lambda-list (find-class 'bar))))
  (is (equal '(request) (http::handler-function-lambda-list (make-instance 'bar))))

  (define-handler bar () ())
  (is (equal nil (http::handler-function bar)))
  (is (equal nil (http::handler-function-lambda-list bar)))

  (define-handler bar () () (:function (lambda (handler request))))
  (is-true (functionp (http::handler-function bar)))
  (is (equal '(handler request) (http::handler-function-lambda-list bar)))

  (signals error (define-handler bar () () (:function (lambda (a b c)))))
  (is-true (functionp (http::handler-function bar)))
  (is (equal '(handler request) (http::handler-function-lambda-list bar))))
