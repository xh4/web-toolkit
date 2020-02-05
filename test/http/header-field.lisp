(in-package :http-test)

(in-suite :http-test)

(defmacro with-read-header-field ((var line) &body body)
  `(with-input-from-lines (stream '(,line))
     (let ((,var (http::read-header-field stream)))
       (is-true (stream-empty-p stream))
       ,@body)))

(test read-header-field
  (with-read-header-field (hf "foo: bar")
    (is-true (typep hf 'header-field))
    (is (equal "foo" (header-field-name hf)))
    (is (equal "bar" (header-field-value hf))))

  (with-read-header-field (hf "foo:bar")
    (is-true (typep hf 'header-field))
    (is (equal "foo" (header-field-name hf)))
    (is (equal "bar" (header-field-value hf))))

  (with-read-header-field (hf "foo:   bar   ")
    (is-true (typep hf 'header-field))
    (is (equal "foo" (header-field-name hf)))
    (is (equal "bar" (header-field-value hf))))

  (with-read-header-field (hf "Host: 127.0.0.1:80")
    (is-true (typep hf 'header-field))
    (is (equal "Host" (header-field-name hf)))
    (is (equal "127.0.0.1:80" (header-field-value hf)))))

(test header-field
  (let ((hf (header-field "foo" "bar")))
    (is (typep hf 'header-field))
    (is (equal "foo" (header-field-name hf)))
    (is (equal "bar" (header-field-value hf))))

  (let ((hf (header-field :foo "bar")))
    (is (typep hf 'header-field))
    (is (equal "Foo" (header-field-name hf)))
    (is (equal "bar" (header-field-value hf))))

  ;; (signals error (header-field "foo"))

  ;; (signals error (header-field 42 "foo"))

  ;; (signals error (header-field "foo" 42))
  )
