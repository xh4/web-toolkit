(in-package :uri-test)

(in-suite :uri-test)

(test percent-decode
  (is (equal (percent-decode nil) nil))
  (is (equal (percent-decode "%63") "c"))
  (is (equal (percent-decode "ab%63") "abc"))
  (is (equal (percent-decode "%E2%9D%A4") "❤"))
  (is (equal (percent-decode "%E7%88%B1") "爱"))
  (is (equal (percent-decode "爱") "爱")))
