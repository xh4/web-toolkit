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
      (is (equal "abc=def" (uri-query uri :type :string)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com/?abc=def")))
      (is (equal "abc=def" (uri-query uri :type :string)))
      (is (equal "/" (uri-path uri)))))

  (it
    (let ((uri (uri::parse-uri "http://coobii.com?abc=def")))
      (is (equal "abc=def" (uri-query uri :type :string)))
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
      (is (equal "bar" (uri-query uri :type :string))))))

(test parse-uri/ip-host
  (it
    (let ((uri (uri::parse-uri "http://1.1.1.1")))
      (is (equal "1.1.1.1" (uri-host uri)))
      ))

  (it
    (let ((uri (uri::parse-uri "http://192.168.42.1")))
      (is (equal "192.168.42.1" (uri-host uri)))))

  (it
    (let ((uri (uri::parse-uri "http://42.42.42.42")))
      (is (equal "42.42.42.42" (uri-host uri))))))

(test parse-uri/ipv6
  (is (equal "fedc:ba98:7654:3210:fedc:ba98:7654:3210"
             (uri-host "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html")))

  (is (equal "1080:0:0:0:8:800:200c:417a"
             (uri-host "http://[1080:0:0:0:8:800:200C:417A]/index.html")))

  (is (equal "3ffe:2a00:100:7031::1"
             (uri-host "http://[3ffe:2a00:100:7031::1]")))

  (is (equal "1080::8:800:200c:417a"
             (uri-host "http://[1080::8:800:200C:417A]/foo")))

  (is (equal "::192.9.5.5"
             (uri-host "http://[::192.9.5.5]/ipng")))

  (is (equal "::ffff:129.144.52.38"
             (uri-host "http://[::FFFF:129.144.52.38]:80/index.html")))

  (is (equal "2010:836b:4179::836b:4179"
             (uri-host "http://[2010:836B:4179::836B:4179]"))))

(test parse-uri/percent-encoding
  (signals error (uri::parse-uri "ht%74p://coobii.com")))

(test parse-uri/invalid
  (signals error (uri::parse-uri "123.example.com:443")))