(in-package :websocket-test)

(in-suite :websocket-test)

(test define-endpoint
  (let ((endpoint (define-endpoint foo-endpoint () ())))
    (is (typep endpoint 'foo-endpoint))
    (is (typep endpoint 'ws::endpoint))
    (is (typep foo-endpoint 'foo-endpoint))
    (is (typep foo-endpoint 'ws::endpoint))))

(test endpoint-is-handler
  (let ((endpoint (define-endpoint foo-endpoint () ())))
    (is (functionp (http::handler-function foo-endpoint)))
    (is (equal 2 (length (http::handler-function-lambda-list foo-endpoint))))))

(test define-endpoint-with-handlers
  (define-endpoint foo-endpoint () () (:on-open (lambda (session))))
  (is (functionp (ws::open-handler foo-endpoint)))
  (is (equal 1 (length (ws::open-handler-lambda-list foo-endpoint)))))
