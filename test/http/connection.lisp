(in-package :http-test)

(in-suite :http-test)

(test persistent-connection
  (with-request-in-stream (stream (make-instance 'request
                                                 :method "GET"
                                                 :uri "/"
                                                 :version "HTTP/1.0"
                                                 :header (header "Connection" "close")))
    (http::read-request stream)))
