(in-package :uri-test)

(in-suite :uri-test)

(test construct-uri
  (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
    (is (typep uri 'uri))
    (is (equal (uri-string uri) "http://coobii.com")))

  (let ((uri (uri::make-uri :scheme "HTTP" :host "Coobii.com")))
    (is (equal (uri-string uri) "http://coobii.com")))

  (let ((uri (uri::make-uri :scheme "http" :host "coobii.com" :port 80)))
    (is (equal (uri-string uri) "http://coobii.com:80")))

  (let ((uri (uri::make-uri :scheme "http" :host "coobii.com" :port "80")))
    (is (equal (uri-string uri) "http://coobii.com:80"))))
