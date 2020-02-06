(in-package :http-test)

(in-suite :http-test)

(defmacro with-read-header ((var lines) &body body)
  `(with-input-from-lines (stream ,lines)
     (let ((,var (http::read-header stream)))
       ,@body)))

(test read-header
  (with-read-header (hd '("foo: bar"))
    (is (typep hd 'header))
    (is (= 1 (length (header-fields hd)))))

  (with-read-header (hd '("foo: bar" ""))
    (is (typep hd 'header))
    (is (= 1 (length (header-fields hd)))))

  (with-read-header (hd '("foo: bar" "" "goo: gle"))
    (is (typep hd 'header))
    (is (= 1 (length (header-fields hd)))))

  (with-read-header (hd '("foo: bar" "goo: gle"))
    (is (typep hd 'header))
    (is (= 2 (length (header-fields hd))))))

(test header
  (let ((header (header "foo" "bar" "goo" "gle")))
    (is (= 2 (length (header-fields header)))))

  (let ((header (header (header-field "foo" "bar"))))
    (is (= 1 (length (header-fields header)))))

  (let ((header (header (header-field "foo" "bar") "goo" "gle")))
    (is (= 2 (length (header-fields header)))))

  (let ((header (header "foo" "bar" (header-field "goo" "gle"))))
    (is (= 2 (length (header-fields header)))))

  ;; (signals error (header "foo"))

  ;; (signals error (header "foo" "bar" "goo"))

  (let ((header (header (list (header-field "foo" "bar")
                              (header-field "goo" "gle")))))
    (is (= 2 (length (header-fields header))))))

(test find-header-field
  (let ((h (header "foo" "bar")))
    (is (equal "bar" (header-field-value (http::find-header-field "foo" h)))))

  (let ((h (header "foo" "bar")))
    (is (equal "bar" (header-field-value (http::find-header-field :foo h))))))

(test set-header-field
  (let ((h (header)))
    (http::set-header-field h (header-field "foo" "bar"))
    (is (equal "bar" (header-field-value (find-header-field "foo" h)))))

  (let ((h (header "foo" "bar")))
    (http::set-header-field h (header-field "foo" "bar2"))
    (is (equal "bar2" (header-field-value (find-header-field :foo h))))))
