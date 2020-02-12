(in-package :http-test)

(in-suite :http-test)

(define-handler bad-handler ()
  ()
  (:function (lambda ()
               (error "oops..."))))

(test handle-error
  (it "should handle error, render for *"
      (let ((response (http::invoke-handler bad-handler
                                            (make-instance 'request
                                                           :header (header "Accept" "*/*")))))
        (is (equal 500 (status-code response)))
        (is (equal "text/html; charset=UTF-8" (header-field-value
                                               (find-header-field
                                                "Content-Type"
                                                (response-header response)))))))

  (it "should handle error, render for html"
    (let ((response (http::invoke-handler bad-handler
                                          (make-instance 'request
                                                         :header (header "Accept" "text/html")))))
      (is (equal 500 (status-code response)))
      (is (equal "text/html; charset=UTF-8" (header-field-value
                                             (find-header-field
                                              "Content-Type"
                                              (response-header response)))))))

  (it "should handle error, render nothing"
      (let ((response (http::invoke-handler bad-handler
                                            (make-instance 'request))))
        (is (equal 500 (status-code response)))
        (is (equal nil (response-body response)))))

  (it "should handle error, render nothing"
      (let ((response (http::invoke-handler bad-handler
                                            (make-instance 'request
                                                           :header (header "Accept" "application/json")))))
        (is (equal 500 (status-code response)))
        (is (equal nil (response-body response))))))
