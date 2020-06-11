(in-package :html-test)

(in-suite :html-test)

(test element
  (is-true (typep (html) 'html))
  (is-true (typep (head) 'head))
  (is-true (typep (body) 'body))
  (is-true (typep (a) 'a))
  (is-true (typep (p) 'p))
  (is-true (typep (h1) 'h1)))

(test attribute
  (let ((el (p :class "foo")))
    (is (equal "foo" (dom:get-attribute el "class"))))

  (let ((el (p :class nil)))
    (is (equal nil (dom:get-attribute el "class")))
    (is-false (dom:has-attribute el "class")))

  (let ((el (p :class t)))
    (is (equal "" (dom:get-attribute el "class")))
    (is-true (dom:has-attribute el "class"))))

(test tag-name
  (is (equal "H1" (dom:tag-name (html:h1)))))

(test local-name
  (is (equal "h1" (dom:local-name (html:h1)))))
