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
    (is-false (dom:has-attribute-p el "class")))

  (let ((el (p :class t)))
    (is (equal "" (dom:get-attribute el "class")))
    (is-true (dom:has-attribute-p el "class"))))

(test children
  (let ((el (p (h1) (h2) (h3))))
    ))
