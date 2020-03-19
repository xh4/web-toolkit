(in-package :reactive-test)

(in-suite :reactive-test)

(test define-variable
  (it
    (ensure-cleanup (foo v/foo)
      (compile-and-load-toplevel-forms
       (define-variable foo 42)
       (is (equal 'utility::variable (type-of v/foo)))
       (is (equal 'foo (utility::variable-name v/foo)))
       (is (equal 42 (utility::variable-form v/foo)))
       (is (equal 42 (utility::variable-value v/foo)))
       (is (equal 42 foo)))))

  (it
    (ensure-cleanup (foo v/foo)
      (compile-and-load-toplevel-forms
       (define-variable foo (* 2 21))
       (is (equal 'utility::variable (type-of v/foo)))
       (is (equal 'foo (utility::variable-name v/foo)))
       (is (equal '(* 2 21) (utility::variable-form v/foo)))
       (is (equal 42 (utility::variable-value v/foo)))
       (is (equal 42 foo))))))

(test define-variable/dependency
  (it
    (ensure-cleanup (foo v/foo bar v/bar)
      (compile-and-load-toplevel-forms
       (define-variable foo 21)
       (define-variable bar (* 2 foo))
       (is (equal 42 bar))
       (is (equal '(* 2 foo) (utility::variable-form v/bar)))
       (is (equal 42 (utility::variable-value v/bar)))
       (is (equal `(,v/foo) (utility::object-dependency/0 v/bar)))
       (is (equal `(,v/bar) (utility::object-propagation/0 v/foo)))))))

(test define-variable/redefine
  (it
    (ensure-cleanup (foo v/foo)
      (compile-and-load-toplevel-forms
       (define-variable foo 21)
       (define-variable foo 42)
       (is (equal 42 foo)))))

  (it
    (ensure-cleanup (foo v/foo)
      (compile-and-load-toplevel-forms
       (define-variable foo (+ 1 1))
       (define-variable foo (+ 2 2))
       (is (equal 4 foo))))))

(test detect-cycle
  (it
    (ensure-cleanup (v1 v/v1 v2 v/v2 v3 v/v3)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (define-variable v3 (1+ v2))
       (finishes (utility::detect-cycle (variable 'v1) nil))
       (signals error
         (utility::detect-cycle (variable 'v1) `(,(variable 'v3))))
       (signals error
         (utility::detect-cycle (variable 'v1) `(,(variable 'v2))))
       (signals error
         (utility::detect-cycle (variable 'v1) `(,(variable 'v1))))))))

(test propagation-list
  (it
    (ensure-cleanup (v1 v/v1 v2 v/v2 v3 v/v3)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (define-variable v3 (1+ v2))
       (is (equal `(,(variable 'v2) ,(variable 'v3))
                  (utility::propagation-list (variable 'v1)))))))

  (it
    (ensure-cleanup (v1 v/v1 v2 v/v2 v3 v/v3 v4 v/v4)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (define-variable v3 (1+ v2))
       (define-variable v4 (1+ v2))
       (is (equal `(,(variable 'v2) ,(variable 'v4) ,(variable 'v3))
                  (utility::propagation-list (variable 'v1))))))))

(test update-variable-value
  (it
    (ensure-cleanup (v1 v/v1)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v1 2)
       (is (equal 2 v1))))))

(test reify
  (it
    (ensure-cleanup (v1 v/v1)
      (compile-and-load-toplevel-forms
       (define-variable v1 (gensym))
       (let ((value-1 v1))
         (utility::reify (variable 'v1))
         (let ((value-2 v1))
           (is-false (equal value-1 value-2))))))))

(test forbid-set-variable-value
  (it
    (signals error
      (ensure-cleanup (v1 v/v1)
        (compile-and-load-toplevel-forms
         (define-variable v1 1)
         (setf v1 2))))))

(test propagation
  (it
    (ensure-cleanup (v1 v/v1 v2 v/v2)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (define-variable v1 2)
       (is (equal 3 v2))))))

(test propagation/error
  (it
    (ensure-cleanup (v1 v/v1 v2 v/v2)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (signals error (define-variable v1 "1"))
       (is (equal v1 1))))))

(test remove-dependency
  (it
    (ensure-cleanup (v1 v/v1 v2 v/v2)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (is (equal `(,(variable 'v1)) (utility::object-dependency/0 (variable 'v2))))
       (define-variable v2 2)
       (is (equal nil (utility::object-dependency/0 (variable 'v2))))))))
