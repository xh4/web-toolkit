(in-package :http-test)

(in-suite :http-test)

(test form-entity
  (let ((entity (http::make-form-entity (form "foo" "bar" "goo" "gle"))))
    (is (equal 'form-entity (type-of entity)))
    (is (equal "application/x-www-form-urlencoded"
               (header-field-value
                (find-header-field :content-type entity))))
    (is (equal 15 (length (http::entity-body entity))))
    (is (equal 'form (type-of (entity-form entity))))))
