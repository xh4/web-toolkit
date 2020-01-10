(in-package :html-test)

(in-suite :html-test)

(test element
  (is (typep (html) 'element))
  (is (typep (head) 'element))
  (is (typep (body) 'element))
  (is (typep (a) 'element))
  (is (typep (p) 'element))
  (is (typep (h1) 'element)))
