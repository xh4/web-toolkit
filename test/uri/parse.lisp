(in-package :uri-test)

(in-suite :uri-test)

(test parse-uri
  (it
    (let ((uri (uri::parse-uri "http://coobii.com")))
      (is (equal "http" (uri-scheme uri)))
      (is (equal "coobii.com" (uri-host uri)))
      (is (equal nil (uri-port uri)))
      (is (equal nil (uri-path uri)))
      (is (equal nil (uri-query uri)))
      (is (equal nil (uri-fragment uri)))))

  (it
    (let ((uri (uri::parse-uri "HTTP://coobii.com")))
      (is (equal "http" (uri-scheme uri)))))

  (it
    (let ((uri (uri::parse-uri "http://xh@coobii.com")))
      (is (equal "xh" (uri-userinfo uri)))))

  (it
    (let ((uri (uri::parse-uri "http://Coobii.com")))
      (is (equal "coobii.com" (uri-host uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com:80")))
      (is (equal 80 (uri-port uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com/")))
      (is (equal "/" (uri-path uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com/foo/bar")))
      (is (equal "/foo/bar" (uri-path uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com/foo/bar?abc=def")))
      (is (equal "abc=def" (uri-query uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com/?abc=def")))
      (is (equal "abc=def" (uri-query uri)))
      (is (equal "/" (uri-path uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com?abc=def")))
      (is (equal "abc=def" (uri-query uri)))
      (is (equal nil (uri-path uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com#fragment")))
      (is (equal "fragment" (uri-fragment uri)))
      (is (equal nil (uri-path uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com/foo/bar#fragment")))
      (is (equal "fragment" (uri-fragment uri)))
      (is (equal "/foo/bar" (uri-path uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com/foo?bar#fragment")))
      (is (equal "fragment" (uri-fragment uri)))
      (is (equal "/foo" (uri-path uri)))
      (is (equal "bar" (uri-query uri))))))

(test parse-uri/percent-encoding
  (signals error (uri::parse-uri "ht%74p://coobii.com"))
  )
