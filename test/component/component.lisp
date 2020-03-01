(in-package :component-test)

(in-suite :component-test)

(test define-component
  (it
    (ensure-cleanup ()
      (define-component foo () () (:tag :div))
      (let ((foo (foo)))
        (is (equal 'foo (type-of foo))))))

  (it "should make root element"
      (ensure-cleanup ()
        (define-component foo () () (:tag :div))
        (let ((foo (foo :data-aaa "bbb" (h1 (h3)) (h2 (h4)))))
          (is (equal 'div (type-of (com::component-root foo)))))))

  (it "should compute-class"
      (ensure-cleanup ()
        (define-component foo () () (:tag :div))
        (define-component bar (foo) () (:tag :div))
        (let ((bar (bar)))
          (is (equal "foo bar" (dom:get-attribute (com::component-root bar) "class"))))))

  (it "should make child elements"
      (ensure-cleanup ()
        (define-component foo () () (:tag :div))
        (let ((foo (foo :data-aaa "bbb" (h1 (h3)) (h2 (h4)))))
          (is (equal 2 (length (com:children foo))))
          (is (equal 'h1 (type-of (first (com:children foo)))))
          (is (equal 'h2 (type-of (second (com:children foo))))))))

  (it "should capture slots & attributes"
      (ensure-cleanup ()
        (define-component foo () ((a-slot)) (:tag :div))
        (let ((foo (foo :data-aaa "bbb" :a-slot "a-slot")))
          (is (equal "a-slot" (slot-value foo 'a-slot)))
          (is (equal "bbb" (dom:get-attribute foo "data-aaa")))))))

(test component-class
  (it
    (ensure-cleanup ()
      (define-component foo () () (:tag :div))
      (define-component bar (foo) () (:tag :div))
      (is (equal '("foo" "bar") (com::compute-component-class (bar)))))))
