(in-package :component-test)

(in-suite :component-test)

(test render
  (it "should return a element"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component com () () (:render (lambda () (html:h1))))
         (let ((com (com)))
           (let ((root (render com)))
             (is (equal 'html:h1 (type-of root))))))))

  (it "should attach to component's root"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component com () () (:render (lambda () (html:h1))))
         (let ((com (com)))
           (render com)
           (is (equal 'html:h1 (type-of (com::component-root com))))))))

  (it "should set component's tag name"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component com () () (:render (lambda () (html:h1))))
         (let ((com (com)))
           (render com)
           (is (equal "h1" (dom:tag-name com)))))))

  (it "should set has component's name as class"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component com () () (:render (lambda () (html:h1))))
         (let ((com (com)))
           (render com)
           (is (equal "com" (dom:get-attribute (com::component-root com) "class")))))))

  (it "should merge component's attributes"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component com () () (:render (lambda () (html:h1))))
         (let ((com (com :class "bar")))
           (render com)
           (is (equal "bar com" (dom:get-attribute (com::component-root com) "class")))))))

  (it "should merge component's attributes"
      (ensure-cleanup (foo)
        (compile-and-load-toplevel-forms
         (define-component com () () (:render (lambda () (html:h1))))
         (let ((com (com :data-foo "foo")))
           (render com)
           (is (equal "foo" (dom:get-attribute (com::component-root com) "data-foo")))))))

  ;; (it "should copy children"
  ;;     (ensure-cleanup (foo)
  ;;       (compile-and-load-toplevel-forms
  ;;        (define-component foo () () (:tag :div))
  ;;        (let ((com (foo (html:h1) (html:h2)))
  ;;              (render (com::make-render '(lambda (com) ))))
  ;;          (let ((children-0 (children com))
  ;;                (children-1 (progn (funcall render com) (children com)))
  ;;                (children-2 (progn (funcall render com) (children com))))
  ;;            (is-false (eq (first children-0) (first children-1)))
  ;;            (is-false (eq (first children-1) (first children-2))))))))

  ;; (it "should be able to use root symbol macro"
  ;;     (ensure-cleanup (foo)
  ;;       (compile-and-load-toplevel-forms
  ;;        (define-component foo () () (:tag :div))
  ;;        (let ((com (foo :class "foo" (html:h1) (html:h2)))
  ;;              (render (com::make-render '(lambda (com) root))))
  ;;          (let ((root (funcall render com)))
  ;;            (is (equal 'html:div (type-of root)))
  ;;            (is (equal "component-test-foo foo" (dom:get-attribute root "class"))))))))

  ;; (it "should be able to use children symbol macro"
  ;;     (ensure-cleanup (foo)
  ;;       (compile-and-load-toplevel-forms
  ;;        (define-component foo () () (:tag :div))
  ;;        (let ((com (foo :class "foo" (html:h1) (html:h2)))
  ;;              (render (com::make-render '(lambda (com) children))))
  ;;          (let ((children (funcall render com)))
  ;;            (is-true (listp children))
  ;;            (is (equal 'html:h1 (type-of (first children))))
  ;;            (is (equal 'html:h2 (type-of (second children)))))))))

  ;; (it "should be able to use root macro (1)"
  ;;     (ensure-cleanup (foo)
  ;;       (compile-and-load-toplevel-forms
  ;;        (define-component foo () () (:tag :div))
  ;;        (let ((com (foo :class "foo" (html:h1) (html:h2)))
  ;;              (render (com::make-render '(lambda (com) (root)))))
  ;;          (let ((root (funcall render com)))
  ;;            (is (equal 'html:div (type-of root)))
  ;;            (is (equal "component-test-foo foo" (dom:get-attribute root "class"))))))))

  ;; (it "should be able to use root macro (2)"
  ;;     (ensure-cleanup (foo)
  ;;       (compile-and-load-toplevel-forms
  ;;        (define-component foo () () (:tag :div))
  ;;        (let ((com (foo :class "foo" (html:h1) (html:h2)))
  ;;              (render (com::make-render '(lambda (com) (root children)))))
  ;;          (let ((root (funcall render com)))
  ;;            (is (equal 'html:div (type-of root)))
  ;;            (is (equal "component-test-foo foo" (dom:get-attribute root "class")))
  ;;            (is (equal 2 (length (children root))))
  ;;            (is (equal 'html:h1 (type-of (first (children root)))))
  ;;            (is (equal 'html:h2 (type-of (second (children root))))))))))

  ;; (it "should be able to use root macro (3)"
  ;;     (ensure-cleanup (foo)
  ;;       (compile-and-load-toplevel-forms
  ;;        (define-component foo () () (:tag :div))
  ;;        (let ((com (foo :class "foo1" (html:h1) (html:h2)))
  ;;              (render (com::make-render '(lambda (com) (root :class "foo2")))))
  ;;          (let ((root (funcall render com)))
  ;;            (is (equal "component-test-foo foo2" (dom:get-attribute root "class"))))))))
  )

(test component-class
  (it
    (ensure-cleanup (foo bar)
      (compile-and-load-toplevel-forms
       (define-component foo () ())
       (define-component bar (foo) ())
       (let ((com (bar)))
         (is (equal '("foo" "bar")
                    (com::compute-component-class (bar))))))))

  (it
    (ensure-cleanup (foo bar)
      (compile-and-load-toplevel-forms
       (define-component foo () ())
       (define-component bar (foo) () (:render (lambda () (html:h1))))
       (let ((com (bar :class "xxx")))
         (render com)
         (is (equal "xxx bar foo" (dom:get-attribute
                                   (com::component-root com)
                                   "class"))))))))
