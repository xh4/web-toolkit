(in-package :dom-test)

(in-suite :dom-test)

(test attributes
  (it
    (let ((el (make-instance 'element :local-name "p")))
      (is-false (has-attributes el))))

  (it
    (let ((el (make-instance 'element :local-name "p")))
      (set-attribute el "foo" "bar")
      (is-true (has-attributes el))
      (is-true (has-attribute el "foo"))
      (is (equal "bar" (get-attribute el "foo")))
      (is (equal '("foo") (get-attribute-names el)))))

  (it
    (let ((el (make-instance 'element :local-name "p")))
      (set-attribute el "foo" "bar")
      (set-attribute el "goo" "gle")
      (is (equal "bar" (get-attribute el "foo")))
      (is (equal "gle" (get-attribute el "goo")))
      (is (equal '("foo" "goo") (get-attribute-names el)))))

  (it
    (let ((el (make-instance 'element :local-name "p")))
      (set-attribute el "foo" "bar")
      (set-attribute el "goo" "gle")
      (remove-attribute el "foo")
      (is-false (has-attribute el "foo"))
      (remove-attribute el "goo")
      (is-false (has-attributes el))))

  (it
    (let ((el (make-instance 'element :local-name "p")))
      (set-attribute el "foo" "bar1")
      (set-attribute el "foo" "bar2")
      (is-true (has-attribute el "foo"))
      (is (equal "bar2" (get-attribute el "foo")))
      (remove-attribute el "foo")
      (is-false (has-attribute el "foo"))
      (is-false (has-attributes el)))))
