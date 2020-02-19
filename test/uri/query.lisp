(in-package :uri-test)

(in-suite :uri-test)

(test uri-query
  (it
    (let ((uri "http://coobii.com?foo=bar"))
      (is (equal "foo" (caar (uri-query uri :type :alist))))
      (is (equal "bar" (cdar (uri-query uri :type :alist))))))

  (it
    (let ((uri "http://coobii.com?foo="))
      (is (equal "" (cdar (uri-query uri :type :alist))))))

  (it
    (let ((uri "http://coobii.com?foo"))
      (is (equal nil (cdar (uri-query uri :type :alist))))))

  (it
    (let ((uri "http://coobii.com?foo&goo=gle"))
      (is (equal nil (cdar (uri-query uri :type :alist))))))

  (it
    (let ((uri "http://coobii.com?foo=%E7%88%B1"))
      (is (equal "爱" (cdar (uri-query uri :type :alist))))))

  (it
    (let ((uri "http://coobii.com?%E7%88%B1=%E4%BD%A0"))
      (is (equal "你" (cdar (uri-query uri :type :alist)))))))

(test uri-query-bad-encoding
  (it
    (let ((uri "//bad?foo=%b6%e0%b3%a1%f1%ee%ba%cf%cf%c2%b6%fe%d1%f5%bb%af%cc%bc%d1%b9%c1%d1%c1%f7%b6%af%b4%ab%c8%c8%d3%eb%d1%d2%ca%af%cb%f0%c9%cb%cc%d8%d0%d4%d1%d0%be%bf_%c0%ee%d0%a1%bd%ad"))
      (is (= 1 (length (uri-query uri :type :alist)))))))
