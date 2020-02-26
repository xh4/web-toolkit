(in-package :http)

(define-condition http-condition () ())

(define-condition http-error (http-condition error) ())
