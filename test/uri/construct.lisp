(in-package :uri-test)

(in-suite :uri-test)

(test construct-uri
  (it
    (let ((uri (uri::make-uri :scheme "http" :host "coobii.com")))
      (is-true (typep uri 'uri))
      (is (equal "http://coobii.com" (uri-string uri)))))

  (signals error (uri::make-uri :scheme "3ww" :host "coobii.com"))

  (it
    (let ((uri (uri::make-uri :scheme "HTTP" :host "Coobii.com")))
      (is (equal "http://coobii.com" (uri-string uri)))))

  (it
    (let ((uri (uri::make-uri :scheme "http" :host "coobii.com" :port 80)))
      (is (equal "http://coobii.com:80" (uri-string uri)))))

  (it
    (let ((uri (uri::make-uri :scheme "http" :host "coobii.com" :port "80")))
      (is (equal "http://coobii.com:80" (uri-string uri))))))
