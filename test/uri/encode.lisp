(in-package :uri-test)

(in-suite :uri-test)

(test percent-encode
  (is (equal nil (uri::percent-encode nil)))
  (is (equal "%E2%9D%A4" (uri::percent-encode "❤")))
  (is (equal "%E7%88%B1" (uri::percent-encode "爱")))
  (is (equal "%61%62%63" (uri::percent-encode "abc")))
  (is (equal "abc" (uri::percent-encode "abc" :reserve 'uri::alpha-p)))
  (is (equal "abc%31%32%33" (uri::percent-encode "abc123" :reserve 'uri::alpha-p)))
  (is (equal "%20" (uri::percent-encode " "))))
