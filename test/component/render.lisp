(in-package :component-test)

(in-suite :component-test)

(test render
  (it "should copy root"
      (define-component foo () () (:tag :div))
      (let ((com (foo))
            (render (com::make-render '(lambda (com) ))))
        (let ((root-0 (root com))
              (root-1 (progn (funcall render com) (root com)))
              (root-2 (progn (funcall render com) (root com))))
          (is-false (eq root-0 root-1))
          (is-false (eq root-1 root-2)))))

  (it "should copy children"
      (define-component foo () () (:tag :div))
      (let ((com (foo (h1) (h2)))
            (render (com::make-render '(lambda (com) ))))
        (let ((children-0 (children com))
              (children-1 (progn (funcall render com) (children com)))
              (children-2 (progn (funcall render com) (children com))))
          (is-false (eq (first children-0) (first children-1)))
          (is-false (eq (first children-1) (first children-2))))))

  (it "should be able to use root symbol macro"
      (define-component foo () () (:tag :div))
      (let ((com (foo :class "foo" (h1) (h2)))
            (render (com::make-render '(lambda (com) root))))
        (let ((root (funcall render com)))
          (is (equal 'div (type-of root)))
          (is (equal "foo" (dom:get-attribute root "class"))))))

  (it "should be able to use children symbol macro"
      (define-component foo () () (:tag :div))
      (let ((com (foo :class "foo" (h1) (h2)))
            (render (com::make-render '(lambda (com) children))))
        (let ((children (funcall render com)))
          (is-true (listp children))
          (is (equal 'h1 (type-of (first children))))
          (is (equal 'h2 (type-of (second children)))))))

  (it "should be able to use root macro (1)"
      (define-component foo () () (:tag :div))
      (let ((com (foo :class "foo" (h1) (h2)))
            (render (com::make-render '(lambda (com) (root)))))
        (let ((root (funcall render com)))
          (is (equal 'div (type-of root)))
          (is (equal "foo" (dom:get-attribute root "class"))))))

  (it "should be able to use root macro (2)"
    (define-component foo () () (:tag :div))
      (let ((com (foo :class "foo" (h1) (h2)))
            (render (com::make-render '(lambda (com) (root children)))))
        (let ((root (funcall render com)))
          (is (equal 'div (type-of root)))
          (is (equal "foo" (dom:get-attribute root "class")))
          (is (equal 2 (length (children root))))
          (is (equal 'h1 (type-of (first (children root)))))
          (is (equal 'h2 (type-of (second (children root)))))))))
