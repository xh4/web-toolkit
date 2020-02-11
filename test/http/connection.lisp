(in-package :http-test)

(in-suite :http-test)

(test persistent-connection
  )

(test handle-request
  (it
    (with-connection (connection)
      (let* ((request (make-instance 'request
                                     :method "GET"
                                     :uri "/"
                                     :version "HTTP/1.0"
                                     :header (header "Connection" "Keep-Alive")))
             (response (http::handle-request connection request)))
        (is (equal "Keep-Alive" (header-field-value
                                 (find-header-field "Connection" response))))))))
