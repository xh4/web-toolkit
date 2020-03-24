(in-package :component-test)

(in-suite :component-test)

(test component-version
  (it
    (ensure-cleanup (com)
      (compile-and-load-toplevel-forms
       (define-component com () ((a :initform "a1")))
       (let ((com (com)))
         (let ((version (com::component-version com)))
           (setf (slot-value com 'a) "a2")
           (is (equal (1+ version) (com::component-version com)))))))))

(test react-to-child-component
  (it
    (ensure-cleanup (com)
      (compile-and-load-toplevel-forms
       (define-component com () ((a :initform 0)))
       (let ((com-1 (com))
             (com-2 (com)))
         (rx:add-dependency com-1 com-2)
         (let ((version (com::component-version com-1)))
           (incf (slot-value com-2 'a))
           (is (equal (1+ version) (com::component-version com-1)))))))))

(test react-to-component-class
  (it
    (ensure-cleanup (com com-1)
      (compile-and-load-toplevel-forms
       (define-component com () ())
       (defvar com-1 (com))
       (let ((version (com::component-version com-1)))
         (define-component com () ())
         (is (equal (1+ version) (com::component-version com-1))))))))
