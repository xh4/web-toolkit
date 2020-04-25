(in-package :javascript-test)

(in-suite :javascript-test)

(test parse-and-serialize-jquery-3-5-0
  (it
   (let* ((response (http:get "http://code.jquery.com/jquery-3.5.0.js"))
          (octets (alexandria::read-stream-content-into-byte-vector
                   (http:response-body response)))
          (source (babel:octets-to-string octets)))
     (unwind-protect
         (test-parse-and-serialize source)
       (close (http:response-body response))))))
