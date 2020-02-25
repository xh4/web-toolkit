(in-package :uri-test)

(in-suite :uri-test)

(test check-scheme
  (is (equal nil (uri::check-scheme nil)))
  (is (equal "www" (uri::check-scheme "www")))
  (is (equal "w3w" (uri::check-scheme "w3w")))
  (is (equal "ww3" (uri::check-scheme "ww3")))
  (is (equal "ftp" (uri::check-scheme "FTP")))
  (is (equal "http" (uri::check-scheme :http)))
  (signals error (uri::check-scheme "3www"))
  (signals error (uri::check-scheme :3www)))

(test check-userinfo
  (is (equal nil (uri::check-userinfo nil)))
  (is (equal "xh" (uri::check-userinfo "xh")))
  (signals error (uri::check-userinfo :xh)))

(test check-host
  (is (equal nil (uri::check-host nil)))
  (is (equal "coobii.com" (uri::check-host "coobii.com")))
  (is (equal "127.0.0.1" (uri::check-host "127.0.0.1"))))

(test check-port
  (is (equal nil (uri::check-port nil)))
  (is (equal 80 (uri::check-port 80)))
  (is (equal 80 (uri::check-port "80")))
  (signals error (uri::check-port "ehh")))

(test check-path
  (is (equal nil (uri::check-path nil))))

(test check-query
  (is (equal nil (uri::check-query nil))))

(test check-fragment
  (is (equal nil (uri::check-fragment nil))))
