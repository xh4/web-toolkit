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

(test header-field
  (it
    (let ((hf (header-field "foo" "bar")))
      (is (equal 'header-field (type-of hf)))
      (is (equal "foo" (header-field-name hf)))
      (is (equal "bar" (header-field-value hf)))))

  (it
    (let ((hf (header-field :content-type "bar")))
      (is (equal 'header-field (type-of hf)))
      (is (equal "Content-Type" (header-field-name hf)))
      (is (equal "bar" (header-field-value hf)))))

  (it
    (let ((hf (header-field :content-length 42)))
      (is (equal 'header-field (type-of hf)))
      (is (equal "Content-Length" (header-field-name hf)))
      (is (equal "42" (header-field-value hf)))))

  (signals error (header-field "foo"))

  (signals error (header-field))

  (signals error (header-field "foo" "bar" "goo")))

(test header
  (it
    (let ((header (header "foo" "bar" "goo" "gle")))
      (is (= 2 (length (header-fields header))))))

  (it
    (let ((header (header "foo" "bar1" "foo" "bar2")))
      (is (= 1 (length (header-fields header))))
      (is (equal "bar2" (header-field-value (find-header-field "foo" header))))))

  (it
    (let ((header (header (header-field "foo" "bar"))))
      (is (= 1 (length (header-fields header))))))

  (it
    (let ((header (header (header-field "foo" "bar") "goo" "gle")))
      (is (= 2 (length (header-fields header))))))

  (it
    (let ((header (header (header-field "foo" "bar1") "foo" "bar2")))
      (is (= 1 (length (header-fields header))))
      (is (equal "bar2" (header-field-value (find-header-field "foo" header))))))

  (it
    (let ((header (header "foo" "bar" (header-field "goo" "gle"))))
      (is (= 2 (length (header-fields header))))))

  (it
    (let ((header (header "foo" "bar1" (header-field "foo" "bar2"))))
      (is (= 1 (length (header-fields header))))
      (is (equal "bar2" (header-field-value (find-header-field "foo" header))))))

  (signals error (header "foo"))

  (signals error (header "foo" "bar" "goo"))

  (it
    (let ((header (header (list (header-field "foo" "bar")
                                (header-field "goo" "gle")))))
      (is (= 2 (length (header-fields header))))))

  (it
    (let ((header (header (list (header-field "foo" "bar1")
                                (header-field "foo" "bar2")))))
      (is (= 1 (length (header-fields header))))
      (is (equal "bar2" (header-field-value (find-header-field "foo" header))))))

  (it
    (let ((header (header
                   (header "foo" "bar"
                           "goo" "gle")
                   (header-field "abc" "def"))))
      (is (= 3 (length (header-fields header))))))

  (it
    (let ((header (header
                   (header "foo" "bar1"
                           "foo" "bar2")
                   (header-field "foo" "bar3"))))
      (is (= 1 (length (header-fields header))))
      (is (equal "bar3" (header-field-value (find-header-field "foo" header))))))

  (it
    (let ((header (header
                   "foo" "bar1"
                   (header "foo" "bar2"
                           "foo" "bar3"))))
      (is (= 1 (length (header-fields header))))
      (is (equal "bar3" (header-field-value (find-header-field "foo" header)))))))

(test find-header-field
  (it
    (let ((h (header "foo" "bar")))
      (is (equal "bar" (header-field-value (http::find-header-field "foo" h))))))

  (it
    (let ((h (header "foo" "bar")))
      (is (equal "bar" (header-field-value (http::find-header-field :foo h))))))

  (it
    (let ((h (header "Content-Type" "bar")))
      (is (equal "bar" (header-field-value (http::find-header-field :content-type h)))))))

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
