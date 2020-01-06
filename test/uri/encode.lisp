(in-package :uri-test)

(in-suite :uri-test)

(test percent-encode
  (is (equal (uri::percent-encode nil) nil))
  (is (equal (uri::percent-encode "❤") "%E2%9D%A4"))
  (is (equal (uri::percent-encode "爱") "%E7%88%B1"))
  (is (equal (uri::percent-encode "abc") "%61%62%63"))
  (is (equal (uri::percent-encode "abc" :reserve 'uri::alpha-p) "abc"))
  (is (equal (uri::percent-encode "abc123" :reserve 'uri::alpha-p)
             "abc%31%32%33")))
