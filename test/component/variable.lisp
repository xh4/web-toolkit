(in-package :component-test)

(in-suite :component-test)

(test define-variable
  (it
    (ensure-variable-cleanup ()
      (define-variable foo 42)
      (is (equal 'com::variable (type-of v/foo)))
      (is (equal 'foo (com::variable-name v/foo)))
      (is (equal 42 (com::variable-form v/foo)))
      (is (equal 42 (com::variable-value v/foo)))
      (is (equal 42 foo))))

  (it
    (ensure-variable-cleanup ()
      (define-variable foo (* 2 21))
      (is (equal 'com::variable (type-of v/foo)))
      (is (equal 'foo (com::variable-name v/foo)))
      (is (equal '(* 2 21) (com::variable-form v/foo)))
      (is (equal 42 (com::variable-value v/foo)))
      (is (equal 42 foo)))))

(test define-variable/dependency
  (it
    (ensure-variable-cleanup ()
      (define-variable foo 21)
      (define-variable bar (* 2 foo))
      (is (equal 42 bar))
      (is (equal '(* 2 foo) (com::variable-form v/bar)))
      (is (equal 42 (com::variable-value v/bar)))
      (is (equal `(,v/foo) (com::variable-dependency v/bar)))
      (is (equal `(,v/bar) (com::variable-propagation v/foo))))))

(test define-variable/redefine
  (it
    (ensure-variable-cleanup ()
      (define-variable foo 21)
      (define-variable foo 42)
      (is (equal 42 foo))))

  (it
    (ensure-variable-cleanup ()
      (define-variable foo (+ 1 1))
      (define-variable foo (+ 2 2))
      (is (equal 4 foo)))))

(test detect-cycle
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (define-variable v2 (1+ v1))
      (define-variable v3 (1+ v2))
      (finishes (com::detect-cycle (variable 'v1) nil))
      (signals error
        (com::detect-cycle (variable 'v1) `(,(variable 'v3))))
      (signals error
        (com::detect-cycle (variable 'v1) `(,(variable 'v2))))
      (signals error
        (com::detect-cycle (variable 'v1) `(,(variable 'v1)))))))

(test propagation-list
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (define-variable v2 (1+ v1))
      (define-variable v3 (1+ v2))
      (is (equal `(,(variable 'v1) ,(variable 'v2) ,(variable 'v3))
                 (com::propagation-list (variable 'v1))))))

  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (define-variable v2 (1+ v1))
      (define-variable v3 (1+ v2))
      (define-variable v4 (1+ v2))
      (is (equal `(,(variable 'v1) ,(variable 'v2) ,(variable 'v4) ,(variable 'v3))
                 (com::propagation-list (variable 'v1)))))))

(test update-variable-value
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (define-variable v1 2))))

(test reinitialize
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 (gensym))
      (let ((value-1 v1))
        (com::reinitialize (variable 'v1))
        (let ((value-2 v1))
          (is-false (equal value-1 value-2)))))))

(test forbid-set-variable-value
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (signals error (setf v1 2)))))

(test propagation
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (define-variable v2 (1+ v1))
      (define-variable v1 2)
      (is (equal 3 v2)))))

(test propagation/error
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (define-variable v2 (1+ v1))
      (signals error (define-variable v1 "1"))
      (is (equal v1 1)))))

(test remove-dependency
  (it
    (ensure-variable-cleanup ()
      (define-variable v1 1)
      (define-variable v2 (1+ v1))
      (is (equal `(,(variable 'v1)) (com::variable-dependency (variable 'v2))))
      (define-variable v2 2)
      (is (equal nil (com::variable-dependency (variable 'v2)))))))
