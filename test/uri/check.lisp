(in-package :uri-test)

(in-suite :uri-test)

(test check-scheme
  (is (equal (uri::check-scheme nil) nil))
  (is (equal (uri::check-scheme "www") "www"))
  (is (equal (uri::check-scheme "w3w") "w3w"))
  (is (equal (uri::check-scheme "ww3") "ww3"))
  (is (equal (uri::check-scheme "FTP") "ftp"))
  (is (equal (uri::check-scheme :http) "http"))
  (signals error (uri::check-scheme "3www"))
  (signals error (uri::check-scheme :3www)))

(test check-userinfo
  (is (equal (uri::check-userinfo nil) nil))
  (is (equal (uri::check-userinfo "xh") "xh"))
  (signals error (uri::check-userinfo :xh)))

(test check-host
  (is (equal (uri::check-host nil) nil))
  (is (equal (uri::check-host "coobii.com") "coobii.com"))
  (is (equal (uri::check-host "127.0.0.1") "127.0.0.1")))

(test check-port
  (is (equal (uri::check-port nil) nil))
  (is (equal (uri::check-port 80) 80))
  (is (equal (uri::check-port "80") 80))
  (signals error (uri::check-port "ehh")))

(test check-path
  (is (equal (uri::check-path nil) nil)))

(test check-query
  (is (equal (uri::check-query nil) nil)))

(test check-fragment
  (is (equal (uri::check-fragment nil) nil)))
