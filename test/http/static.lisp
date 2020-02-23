(in-package :http-test)

(in-suite :http-test)

(test path-trim-prefix
  (is (equal "" (http::path-trim-prefix "foo" "/foo")))
  (is (equal "" (http::path-trim-prefix "foo" "/foo/")))
  (is (equal "bar" (http::path-trim-prefix "foo" "/foo/bar")))
  (is (equal "bar" (http::path-trim-prefix "/foo" "/foo/bar")))
  (is (equal "bar" (http::path-trim-prefix "foo/" "/foo/bar")))
  (is (equal "bar" (http::path-trim-prefix "/foo/" "/foo/bar")))
  (is (equal "" (http::path-trim-prefix "/foo/bar" "/foo/bar")))
  (is (equal "" (http::path-trim-prefix "/foo/bar" "/foo/bar/")))
  (is (equal "" (http::path-trim-prefix "/foo/bar/" "/foo/bar")))
  (is (equal "" (http::path-trim-prefix "/foo/bar/" "/foo/bar/")))
  (is (equal nil (http::path-trim-prefix "goo" "/foo/bar/")))
  (is (equal "foo/bar/" (http::path-trim-prefix "/" "/foo/bar/")))
  (is (equal "foo/bar" (http::path-trim-prefix "" "/foo/bar")))
  (is (equal "foo/bar/" (http::path-trim-prefix nil "/foo/bar/"))))

(test path-trim-prefix/with-dots
  (is (equal "" (http::path-trim-prefix "" "/..")))
  (is (equal "foo" (http::path-trim-prefix "" "/../foo")))
  (is (equal "foo" (http::path-trim-prefix "" "/.././foo")))
  (is (equal "foo" (http::path-trim-prefix "" "/../../foo")))
  (is (equal nil (http::path-trim-prefix "foo" "/../foo/../bar"))))

(test path-prefix-p
  (is-true (http::path-prefix-p "" "/"))
  (is-true (http::path-prefix-p "/" "/"))
  (is-true (http::path-prefix-p "foo" "/foo"))
  (is-true (http::path-prefix-p "foo/" "/foo"))
  (is-true (http::path-prefix-p "/foo" "/foo"))
  (is-true (http::path-prefix-p "/foo/" "/foo"))
  (is-true (http::path-prefix-p "/foo" "/foo/"))
  (is-true (http::path-prefix-p "/foo/" "/foo/"))
  (is-true (http::path-prefix-p "/foo" "/foo/bar"))
  (is-true (http::path-prefix-p "/foo/" "/foo/bar"))
  (is-true (http::path-prefix-p "/foo/bar" "/foo/bar"))
  (is-true (http::path-prefix-p "/foo/bar" "/foo/bar/"))
  (is-true (http::path-prefix-p "/foo/bar/" "/foo/bar/")))

(test path-prefix-p/with-dots
  (is-true (http::path-prefix-p "" "/.."))
  (is-true (http::path-prefix-p "/" "/.."))
  (is-true (http::path-prefix-p "foo" "/../foo"))
  (is-true (http::path-prefix-p "foo" "/foo/."))
  (is-true (http::path-prefix-p "foo/" "/.././foo"))
  (is-true (http::path-prefix-p "/foo" "/../foo"))
  (is-true (http::path-prefix-p "/foo/" "/../foo"))
  (is-true (http::path-prefix-p "/foo" "/bar/../foo/"))
  (is-true (http::path-prefix-p "/foo/" "/../foo/"))
  (is-true (http::path-prefix-p "/foo" "/foo/bar/."))
  (is-true (http::path-prefix-p "/foo/bar" "/foo/bar/."))
  (is-true (http::path-prefix-p "/foo/bar" "/foo/bar/../bar")))

(test resolve-path
  #+os-windows
  (it "should work on windows"
      (is (equal "C:\\" (namestring (http::resolve-path "" "C:/" "/"))))
      (is (equal "C:\\" (namestring (http::resolve-path "" "C:\\" "/"))))
      (is (equal "C:\\abc" (namestring (http::resolve-path "" "C:/" "/abc"))))
      (is (equal "C:\\abc" (namestring (http::resolve-path "" "C:\\" "/abc"))))
      (is (equal "C:\\abc\\" (namestring (http::resolve-path "" "C:/" "/abc/"))))
      (is (equal "C:\\abc\\def" (namestring (http::resolve-path "" "C:/" "/abc/def"))))
      (is (equal "C:\\abc\\def\\" (namestring (http::resolve-path "" "C:/" "/abc/def/"))))
      (is (equal "C:\\" (namestring (http::resolve-path "abc" "C:/" "/abc"))))
      (is (equal "C:\\" (namestring (http::resolve-path "/abc" "C:/" "/abc/"))))
      (is (equal "C:\\" (namestring (http::resolve-path "/abc/" "C:/" "/abc/"))))
      (is (equal "C:\\def" (namestring (http::resolve-path "/abc/" "C:/" "/abc/def"))))
      (is (equal "C:\\def\\" (namestring (http::resolve-path "/abc/" "C:/" "/abc/def/")))))
  #-os-windows
  (it "should work on linux"
      ))

(test resolve-path/with-dots
  #+os-windows
  (it "should work on windows"
      (is (equal "C:\\" (namestring (http::resolve-path "" "C:/" "/.."))))
      (is (equal "C:\\abc\\" (namestring (http::resolve-path "" "C:/" "/abc/."))))
      (is (equal "C:\\abc\\" (namestring (http::resolve-path "" "C:/" "/abc/def/..")))))
  #-os-windows
  (it "should work on linux"
      ))

(test resolve-path/percent-encoding
  #+os-windows
  (it "should work on windows"
      (is (equal "C:\\你好.txt" (namestring (http::resolve-path "" "C:/" "/%E4%BD%A0%E5%A5%BD.txt")))))
  #-os-windows
  (it "should work on linux"
      ))

(test static-route-match-p
  (it
    (with-static-files (root ("foo" "bar"))
      (let ((route (http::make-static-route :root root)))
        (is-true (http::route-match-p route (make-instance 'request :method :get :uri "/")))
        (is-false (http::route-match-p route (make-instance 'request :method :post :uri "/")))
        (is-true (http::route-match-p route (make-instance 'request :method :get :uri "/foo")))
        (is-false (http::route-match-p route (make-instance 'request :method :get :uri "/xxx")))))))

(test auto-redirect-to-directory
      (it
       (with-static-files (root ("foo" "bar"))
         (let* ((route (http::make-static-route :root root))
                (handler (http::route-handler route)))
           (let ((response (http::invoke-handler handler (make-instance 'request :method :get :uri "/foo"))))
             (is (equal 307 (status-code response)))
             (is (equal "/foo/" (header-field-value (find-header-field "Location" response))))))))
      (it
        (with-static-files (root ("foo" "bar"))
          (let* ((route (http::make-static-route :root root))
                 (handler (http::route-handler route)))
            (let ((response (http::invoke-handler handler (make-instance 'request :method :get :uri "/foo/"))))
              (is (equal 200 (status-code response)))
              (is (equal "text/html; charset=UTF-8" (header-field-value
                                                     (find-header-field
                                                      "Content-Type"
                                                      (response-header response)))))))))
      (it
        (with-static-files (root ("foo" "bar"))
          (let* ((route (http::make-static-route :root root))
                 (handler (http::route-handler route)))
            (let ((response (http::invoke-handler handler (make-instance 'request :method :get :uri "/foo/bar"))))
              (is (equal 200 (status-code response)))
              (is (equal 'pathname (type-of (response-body response)))))))))

(test make-static-route
  (it
    (let ((route (route :static '(:static :prefix "/" :root #p"/"))))
      (is (equal 'http::static-route (type-of route))))

    (let ((route (route :static '(:static
                                  :prefix (format nil "/~A" "foo")
                                  :root (merge-pathnames
                                         #p"foo/"
                                         #p"/")))))
      (is (equal 'http::static-route (type-of route))))))
