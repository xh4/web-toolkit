(in-package :http-test)

(in-suite :http-test)

(test redirect
  (it "should redirect"
      (define-handler foo ()
        ()
        (:function (lambda ()
                     (redirect "/foo")
                     (reply (status 200)))))

      (let ((res (http::invoke-handler foo nil)))
        (is (typep res 'response))
        (is (equal 307 (status-code (response-status res))))
        (is (equal "/foo" (header-field-value
                           (find-header-field
                            "Location"
                            res)))))))
