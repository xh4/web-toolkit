(in-package :uri-test)

(in-suite :uri-test)

(test percent-encode
  (is (equal (uri::percent-encode "❤") "%E2%9D%A4"))
  (is (equal (uri::percent-encode "爱") "%E7%88%B1")))
