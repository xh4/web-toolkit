(in-package :uri-test)

(in-suite :uri-test)

(test update-uri
  (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
    (setf (uri-scheme uri) "ftp")
    (is (equal (uri-scheme uri) "ftp"))
    (signals error (setf (uri-scheme uri) "3ww")))

  (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
    (setf (uri-userinfo uri) "xh")
    (is (equal (uri-userinfo uri) "xh")))

  (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
    (setf (uri-userinfo uri) "贺")
    (is (equal (uri-userinfo uri) "贺")))

  (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
    (setf (uri-host uri) "xh.coobii.com")
    (is (equal (uri-host uri) "xh.coobii.com"))))
