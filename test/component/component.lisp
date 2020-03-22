(in-package :component-test)

(in-suite :component-test)

(test define-component
  (it "should has own class"
    (ensure-cleanup (foo)
      (compile-and-load-toplevel-forms
       (define-component foo () ())
       (let ((foo (foo)))
         (is (equal 'foo (type-of foo)))))))

  (it "should be a subclass of custom-element"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component foo () ())
         (let ((foo (foo)))
           (is-true (typep foo 'html:custom-element)))))))

(test construct-component
  (it "should distinguishes slots arguments from attributes arguments"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component foo () ((a-slot :initarg :a-slot)))
         (let ((foo (foo :data-attr "attr" :a-slot "a-slot")))
           (is (equal "a-slot" (slot-value foo 'a-slot)))
           (is (equal "attr" (dom:get-attribute foo "data-attr")))))))

  (it "should allow child elements"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component foo () ())
         (let ((foo (foo (html:h1 (html:h3))
                         (html:h2 (html:h4)))))
           (is (equal 2 (length (dom:children foo))))
           (is (equal 'html:h1 (type-of (first (dom:children foo)))))
           (is (equal 'html:h2 (type-of (second (dom:children foo))))))))))
