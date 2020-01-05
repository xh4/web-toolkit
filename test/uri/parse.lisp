(in-package :uri-test)

(in-suite :uri-test)

(test parse-uri
  (let ((uri (uri::parse-uri "http://coobii.com")))
    (is (equal (uri-scheme uri) "http"))
    (is (equal (uri-host uri) "coobii.com")))

  (let ((uri (uri::parse-uri "http://xh@coobii.com")))
    (is (equal (uri-userinfo uri) "xh")))

  (let ((uri (uri::parse-uri "http://coobii.com:80")))
    (is (equal (uri-port uri) 80)))

  (let ((uri (uri::parse-uri "http://coobii.com/foo/bar")))
    (is (equal (uri-path uri) "/foo/bar")))

  (let ((uri (uri::parse-uri "http://coobii.com/foo/bar?abc=def")))
    (is (equal (uri-query uri) "abc=def")))

  (let ((uri (uri::parse-uri "http://coobii.com/?abc=def")))
    (is (equal (uri-query uri) "abc=def")))

  (let ((uri (uri::parse-uri "http://coobii.com?abc=def")))
    (is (equal (uri-query uri) "abc=def")))

  (let ((uri (uri::parse-uri "http://coobii.com#fragment")))
    (is (equal (uri-fragment uri) "fragment")))

  (let ((uri (uri::parse-uri "http://coobii.com/foo/bar#fragment")))
    (is (equal (uri-fragment uri) "fragment")))

  (let ((uri (uri::parse-uri "http://coobii.com/foo?bar#fragment")))
    (is (equal (uri-fragment uri) "fragment"))))
