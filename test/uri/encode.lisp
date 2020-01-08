(in-package :uri-test)

(in-suite :uri-test)

(test percent-encode
  (is (equal (percent-encode nil) nil))
  (is (equal (percent-encode "❤") "%E2%9D%A4"))
  (is (equal (percent-encode "爱") "%E7%88%B1"))
  (is (equal (percent-encode "abc") "%61%62%63"))
  (is (equal (percent-encode "abc" :reserve 'uri::alpha-p) "abc"))
  (is (equal (percent-encode "abc123" :reserve 'uri::alpha-p)
             "abc%31%32%33")))
