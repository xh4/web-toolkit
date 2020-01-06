(in-package :uri-test)

(in-suite :uri-test)

(test percent-decode
  (is (equal (uri::percent-decode "%63") "c"))
  (is (equal (uri::percent-decode "ab%63") "abc"))
  (is (equal (uri::percent-decode "%E2%9D%A4") "❤"))
  (is (equal (uri::percent-decode "%E7%88%B1") "爱")))
