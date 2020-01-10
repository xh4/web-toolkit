(in-package :http-test)

(in-suite :http-test)

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
