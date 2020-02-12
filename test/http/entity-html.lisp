(in-package :http-test)

(in-suite :http-test)

(test html-entity
  (it
    (let ((entity (make-instance 'http::html-entity
                                 :body (html:h1 "foo"))))
      (is (equal 12 (length (response-body entity))))
      (is (equal "text/html; charset=UTF-8" (header-field-value
                                             (find-header-field
                                              "Content-Type"
                                              (response-header entity)))))))

  (it
    (let ((entity (make-instance 'http::html-entity
                                 :body (html:h1 "你好"))))
      (is (equal 15 (length (response-body entity)))))))
