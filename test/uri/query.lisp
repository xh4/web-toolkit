(in-package :uri-test)

(in-suite :uri-test)

(test uri-query
  (it
    (let ((uri "http://coobii.com?foo=bar"))
      (is (equal "foo" (caar (uri-query uri))))
      (is (equal "bar" (cdar (uri-query uri))))))

  (it
    (let ((uri "http://coobii.com?foo="))
      (is (equal "" (cdar (uri-query uri))))))

  (it
    (let ((uri "http://coobii.com?foo"))
      (is (equal nil (cdar (uri-query uri))))))

  (it
    (let ((uri "http://coobii.com?foo&goo=gle"))
      (is (equal nil (cdar (uri-query uri))))))

  (it
    (let ((uri "http://coobii.com?foo=%E7%88%B1"))
      (is (equal "爱" (cdar (uri-query uri))))))

  (it
    (let ((uri "http://coobii.com?%E7%88%B1=%E4%BD%A0"))
      (is (equal "你" (cdar (uri-query uri))))))

  (it
    (let ((uri "http://coobii.com?%E7%88%B1=%E4%BD%A0"))
      (is (equal "%E7%88%B1=%E4%BD%A0" (uri-query uri :type nil)))))

  (it
    (let ((uri "http://coobii.com?%E7%88%B1=%E4%BD%A0"))
      (is (equal "爱=你" (uri-query uri :type nil :decode t))))))

(test uri-query-bad-encoding
  (it
    (let ((uri "//bad?foo=%b6%e0%b3%a1%f1%ee%ba%cf%cf%c2%b6%fe%d1%f5%bb%af%cc%bc%d1%b9%c1%d1%c1%f7%b6%af%b4%ab%c8%c8%d3%eb%d1%d2%ca%af%cb%f0%c9%cb%cc%d8%d0%d4%d1%d0%be%bf_%c0%ee%d0%a1%bd%ad"))
      (is (= 1 (length (uri-query uri :type :alist)))))))

(test alist-query
  (is (equal "" (uri::alist-query '(("foo")))))

  (is (equal "foo" (uri::alist-query '(("foo" . "")))))

  (is (equal "foo=bar" (uri::alist-query '(("foo" . "bar")))))

  (is (equal "foo=4" (uri::alist-query '(("foo" . 4)))))

  (is (equal "foo=BAR" (uri::alist-query '(("foo" . bar))))))

(test get-query-parameter-empty-key
  (it
    (let ((query (uri-query "http://www.google.com/?=b&")))
      (is (equal 2 (length query)))
      (is (equal "b" (cdr (first query))))
      (is (equal nil (cdr (second query)))))))

(test get-query-parameter-empty-key-2
  (it
    (let ((query (uri-query "http://www.google.com/?")))
      (is (equal 1 (length query)))
      (is (equal "" (car (first query))))
      (is (equal nil (cdr (first query)))))))

(test get-query-parameter-names-edge-cases
  (it
    (let ((query (uri-query "http://foo?a=bar&b=bar&c=&&d=baz&e&f&g=buzz&&&a&b=bar&h")))
      (is (equal 9 (length (remove-duplicates
                            query
                            :test 'equal :key 'car)))))))

(test get-query-parameter-escaped-keys
  (is (equal "foo" (cdr (assoc "a b"
                               (uri-query "http://www.google.com/?a%20b=foo&c%20d=")
                               :test 'equal)))))
