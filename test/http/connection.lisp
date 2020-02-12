(in-package :http-test)

(in-suite :http-test)

(test persistent-connection-1.0
  (it "should close the connection by default"
      (let ((res (process-request (list (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.0")
                                        (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.0")))))
        (is (equal 1 (length res)))))

  (it "should close the connection if client ask for it"
    (let ((res (process-request (list (make-instance 'request
                                                     :method "GET"
                                                     :uri "/"
                                                     :version "HTTP/1.0"
                                                     :header (header "Connection" "close"))
                                      (make-instance 'request
                                                     :method "GET"
                                                     :uri "/"
                                                     :version "HTTP/1.0")))))
      (is (equal 1 (length res)))))

  (it "should keep alive when client ask it"
      (let ((res (process-request (list (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.0"
                                                       :header (header "Connection" "Keep-Alive"))
                                        (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.0")))))
        (is (equal 2 (length res))))))

(test persistent-connection-1.1
  (it "should keep alive by default"
      (let ((res (process-request (list (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.1")
                                        (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.1")))))
        (is (equal 2 (length res)))))

  (it "should close connection if client ask for it"
      (let ((res (process-request (list (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.1"
                                                       :header (header "Connection" "close"))
                                        (make-instance 'request
                                                       :method "GET"
                                                       :uri "/"
                                                       :version "HTTP/1.1")))))
        (is (equal 1 (length res))))))
