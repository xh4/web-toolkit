(in-package :uri-test)

(in-suite :uri-test)

(test update-uri
  (it
    (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
      (setf (uri-scheme uri) "ftp")
      (is (equal "ftp" (uri-scheme uri)))
      (signals error (setf (uri-scheme uri) "3ww"))))

  (it
    (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
      (setf (uri-userinfo uri) "xh")
      (is (equal "xh" (uri-userinfo uri)))))

  (it
    (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
      (setf (uri-userinfo uri) "贺")
      (is (equal "贺" (uri-userinfo uri)))))

  (it
    (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
      (setf (uri-host uri) "xh.coobii.com")
      (is (equal "xh.coobii.com" (uri-host uri))))))
