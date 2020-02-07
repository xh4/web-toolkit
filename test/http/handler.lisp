(in-package :http-test)

(in-suite :http-test)

(test define-handler-with-instalize-nil
  (let ((handler-class (define-handler foo () () (:instanize nil))))
    (is (typep handler-class 'http::handler-class))))

(test define-handler-with-instalize-unset
  (let ((handler (define-handler foo () ())))
    (is (typep handler 'http::handler))))

(test define-handler-with-instalize-nil-and-a-function
  (define-handler test-handler () () (:function (lambda (request))) (:instanize nil))
  (is-true (functionp (http::handler-function (find-class 'test-handler))))
  (is-true (functionp (http::handler-function (make-instance 'test-handler))))
  (is (equal '(request) (http::handler-function-lambda-list (find-class 'test-handler))))
  (is (equal '(request) (http::handler-function-lambda-list (make-instance 'test-handler)))))

(test define-handler-with-instalize-nil-and-a-function-then-change-the-function
  (define-handler test-handler () () (:function (lambda (request))) (:instanize nil))
  (define-handler test-handler () () (:function (lambda (handler request))) (:instanize nil))
  (is (equal '(handler request) (http::handler-function-lambda-list (find-class 'test-handler))))
  (is (equal '(handler request) (http::handler-function-lambda-list (make-instance 'test-handler)))))

(test define-handler-with-instalize-t-without-function
  (define-handler test-handler () () (:instanize t))
  (is (equal nil (http::handler-function test-handler)))
  (is (equal nil (http::handler-function-lambda-list test-handler))))

(test define-handler-with-instalize-t-with-a-function
  (define-handler test-handler () () (:function (lambda (request))) (:instanize t))
  (is-true (functionp (http::handler-function test-handler)))
  (is (equal '(request) (http::handler-function-lambda-list test-handler))))

(test define-handler-with-instalize-t-with-a-function-then-change-the-function
  (define-handler test-handler () () (:function (lambda (request))) (:instanize t))
  (define-handler test-handler () () (:function (lambda (handler request))) (:instanize t))
  (is-true (functionp (http::handler-function test-handler)))
  (is (equal '(handler request) (http::handler-function-lambda-list test-handler))))

(test define-handler-with-wrong-function-arity
  (define-handler test-handler () () (:function (lambda (handler request))))
  (signals error (define-handler test-handler () () (:function (lambda (a b c)))))
  ;; Keep function & function-lambda-list untuoched
  (is-true (functionp (http::handler-function test-handler)))
  (is (equal '(handler request) (http::handler-function-lambda-list test-handler))))

(test define-handler-with-wrong-function-type
  (define-handler test-handler () () (:function (lambda (handler request))))
  (signals error (define-handler test-handler () () (:function 42)))
  ;; Keep function & function-lambda-list untuoched
  (is-true (functionp (http::handler-function test-handler)))
  (is (equal '(handler request) (http::handler-function-lambda-list test-handler))))

(test define-handler-with-empty-function
  (define-handler test-handler () () (:function))
  (is-true (null (http::handler-function test-handler)))
  (is-true (null (http::handler-function-lambda-list test-handler))))

(test define-handler-with-call-cc-function
  (define-handler test-handler () () (:function (cl-cont:lambda/cc (request))))
  (is-true (typep (http::handler-function test-handler) 'cl-cont::funcallable/cc))
  (is (equal '(request) (http::handler-function-lambda-list test-handler))))
