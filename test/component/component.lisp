(in-package :component-test)

(in-suite :component-test)

(test define-component
  (it
    (define-component foo () () (:tag :div))
    (let ((foo (foo)))
      (is (equal 'foo (type-of foo)))))

  (it "should make root element"
      (define-component foo () () (:tag :div))
      (let ((foo (foo :data-aaa "bbb" (h1 (h3)) (h2 (h4)))))
        (is (equal 'div (type-of (com::component-root foo))))))

  (it "should make child elements"
      (define-component foo () () (:tag :div))
      (let ((foo (foo :data-aaa "bbb" (h1 (h3)) (h2 (h4)))))
        (is (equal 2 (length (com:children foo))))
        (is (equal 'h1 (type-of (first (com:children foo)))))
        (is (equal 'h2 (type-of (second (com:children foo))))))))
