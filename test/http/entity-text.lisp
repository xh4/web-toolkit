(in-package :http-test)

(in-suite :http-test)

(test text-entity
  (it
    (let ((entity (http::make-text-entity "foo")))
      (is (equal 3 (length (response-body entity))))
      (is (equal "text/plain; charset=UTF-8" (header-field-value
                                              (find-header-field
                                               "Content-Type"
                                               (response-header entity)))))))

  (it
    (let ((entity (http::make-text-entity "你好")))
      (is (equal 6 (length (response-body entity)))))))
