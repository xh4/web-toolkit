(in-package :http-test)

(in-suite :http-test)

(test serialize-form
  (it
    (let* ((form (form "foo" "bar"))
           (data (http::serialize-form form)))
      (is-true (typep data 'vector))
      (is (equal "foo=bar" (babel:octets-to-string data)))))

  (it
    (let* ((form (form "foo" "bar" "goo" "gle"))
           (data (http::serialize-form form)))
      (is-true (typep data 'vector))
      (is (equal "foo=bar&goo=gle" (babel:octets-to-string data))))))
