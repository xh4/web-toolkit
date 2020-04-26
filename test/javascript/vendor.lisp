(in-package :javascript-test)

(in-suite :javascript-test)

(test parse-and-serialize-jquery-3-5-0
  (it
    (let* ((response (http:get "http://code.jquery.com/jquery-3.5.0.js"))
           (octets (http::read-response-body-into-vector response))
           (source (babel:octets-to-string octets)))
      (unwind-protect
          (test-parse-and-serialize source)
        (close (http:response-body response))))))

(test parse-and-serialize-react-16-13-1
  (it
    (let* ((response (http:get "https://unpkg.com/react@16.13.1/umd/react.development.js"))
           (octets (http::read-response-body-into-vector response))
           (source (babel:octets-to-string octets)))
      (unwind-protect
          (test-parse-and-serialize source)
        (close (http:response-body response))))))

(test parse-and-serialize-react-dom-16-13-1
  (it
    (let* ((response (http:get "https://unpkg.com/react-dom@16.13.1/umd/react-dom.development.js"))
           (octets (http::read-response-body-into-vector response))
           (source (babel:octets-to-string octets)))
      (unwind-protect
          (test-parse-and-serialize source)
        (close (http:response-body response))))))


