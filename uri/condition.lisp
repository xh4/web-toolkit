(in-package :uri)

(define-condition uri-condition () ())

(define-condition uri-error (uri-condition error) ())

(define-condition uri-parsing-error (uri-error)
  ((uri-string
    :initarg :uri-string
    :initform nil)))
