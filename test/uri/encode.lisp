(in-package :uri-test)

(in-suite :uri-test)

(test percent-encode
  (is (equal nil (percent-encode nil)))
  (is (equal "%E2%9D%A4" (percent-encode "❤")))
  (is (equal "%E7%88%B1" (percent-encode "爱")))
  (is (equal "%61%62%63" (percent-encode "abc")))
  (is (equal "abc" (percent-encode "abc" :reserve 'uri::alpha-p)))
  (is (equal "abc%31%32%33" (percent-encode "abc123" :reserve 'uri::alpha-p)))
  (is (equal "%20" (percent-encode " "))))
