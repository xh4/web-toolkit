(in-package :uri-test)

(in-suite :uri-test)

(test construct-uri
  (let ((uri (make-instance 'uri :scheme "http" :host "coobii.com")))
    (is (typep uri 'uri))
    (is (equal (uri-string uri) "http://coobii.com"))))
