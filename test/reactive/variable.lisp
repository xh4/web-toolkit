(in-package :reactive-test)

(in-suite :reactive-test)

(test define-variable
  (it
    (ensure-variable-cleanup (foo)
      (compile-and-load-toplevel-forms
       (define-variable foo 42)
       (is (equal 'rx:variable (type-of (variable 'foo))))
       (is (equal 'foo (variable-name (variable 'foo))))
       (is (equal 42 (variable-form (variable 'foo))))
       (is (equal 42 (variable-value (variable 'foo))))
       (is (equal 42 foo)))))

  (it
    (ensure-variable-cleanup (foo)
      (compile-and-load-toplevel-forms
       (define-variable foo (* 2 21))
       (is (equal 'rx:variable (type-of (variable 'foo))))
       (is (equal 'foo (variable-name (variable 'foo))))
       (is (equal '(* 2 21) (variable-form (variable 'foo))))
       (is (equal 42 (variable-value (variable 'foo))))
       (is (equal 42 foo))))))

(test define-variable/dependency
  (it
    (ensure-variable-cleanup (foo bar)
      (compile-and-load-toplevel-forms
       (define-variable foo 21)
       (define-variable bar (* 2 foo))
       (is (equal 42 bar))
       (is (equal '(* 2 foo) (variable-form (variable 'bar))))
       (is (equal 42 (variable-value (variable 'bar))))
       (is-true (find (variable 'foo) (rx::object-dependency (variable 'bar))))
       (is-true (find (variable 'bar) (rx::object-propagation (variable 'foo))))))))

(test define-variable/redefine
  (it
    (ensure-variable-cleanup (foo)
      (compile-and-load-toplevel-forms
       (define-variable foo 21)
       (define-variable foo 42)
       (is (equal 42 foo)))))

  (it
    (ensure-variable-cleanup (foo)
      (compile-and-load-toplevel-forms
       (define-variable foo (+ 1 1))
       (define-variable foo (+ 2 2))
       (is (equal 4 foo))))))

(test detect-cycle
  (it
    (ensure-variable-cleanup (v1 v2)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (define-variable v1 (1+ v2))
       (is (equal 3 v1))))))

(test update-variable-value
  (it
    (ensure-variable-cleanup (v1)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v1 2)
       (is (equal 2 v1))))))

(test reify
  (it
    (ensure-variable-cleanup (v1)
      (compile-and-load-toplevel-forms
       (define-variable v1 (gensym))
       (let ((value-1 v1))
         (rx::reify (variable 'v1))
         (let ((value-2 v1))
           (is-false (equal value-1 value-2))))))))

(test forbid-set-variable-value
  (it
    (signals error
      (ensure-variable-cleanup (v1)
        (compile-and-load-toplevel-forms
         (define-variable v1 1)
         (setf v1 2))))))

(test propagation
  (it
    (ensure-variable-cleanup (v1 v2)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (define-variable v1 2)
       (is (equal 3 v2))))))

(test propagation/error
  (it
    (ensure-variable-cleanup (v1 v2)
      (compile-and-load-toplevel-forms
       (define-variable v1 1)
       (define-variable v2 (1+ v1))
       (signals error (define-variable v1 "1"))
       (is (equal v1 1))))))
