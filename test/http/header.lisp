(in-package :http-test)

(in-suite :http-test)

(test read-header
  (it
    (with-read-header (hd '("foo: bar"))
      (is (typep hd 'header))
      (is (= 1 (length (header-fields hd))))))

  (it
    (with-read-header (hd '("foo: bar" ""))
      (is (typep hd 'header))
      (is (= 1 (length (header-fields hd))))))

  (it
    (with-read-header (hd '("foo: bar" "" "goo: gle"))
      (is (typep hd 'header))
      (is (= 1 (length (header-fields hd))))))

  (it
    (with-read-header (hd '("foo: bar" "goo: gle"))
      (is (typep hd 'header))
      (is (= 2 (length (header-fields hd)))))))

(test header
  (it
    (let ((header (header "foo" "bar" "goo" "gle")))
      (is (= 2 (length (header-fields header))))))

  (it
    (let ((header (header (header-field "foo" "bar"))))
      (is (= 1 (length (header-fields header))))))

  (it
    (let ((header (header (header-field "foo" "bar") "goo" "gle")))
      (is (= 2 (length (header-fields header))))))

  (it
    (let ((header (header "foo" "bar" (header-field "goo" "gle"))))
      (is (= 2 (length (header-fields header))))))

  ;; (signals error (header "foo"))

  ;; (signals error (header "foo" "bar" "goo"))

  (it
    (let ((header (header (list (header-field "foo" "bar")
                                (header-field "goo" "gle")))))
      (is (= 2 (length (header-fields header)))))))

(test find-header-field
  (it
    (let ((h (header "foo" "bar")))
      (is (equal "bar" (header-field-value (http::find-header-field "foo" h))))))

  (it
    (let ((h (header "foo" "bar")))
      (is (equal "bar" (header-field-value (http::find-header-field :foo h)))))))

(test set-header-field
  (it
    (let ((h (header)))
      (http::set-header-field h (header-field "foo" "bar"))
      (is (equal "bar" (header-field-value (find-header-field "foo" h))))))

  (it
    (let ((h (header "foo" "bar")))
      (http::set-header-field h (header-field "foo" "bar2"))
      (is (equal "bar2" (header-field-value (find-header-field :foo h)))))))

(test write-header
  (it
    (let ((lines (with-output-to-string (stream)
                   (http::write-header stream (header "foo" "bar"
                                                      "goo" "gle")))))
      (let ((last-4-chars (subseq lines (- (length lines) 4) (length lines))))
        (is (equal (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline)
                   last-4-chars))))))
