(in-package :dom-test)

(in-suite :dom-test)

(test root
  (it
    (with-nodes (a (b (c d)))
      (is (eq a (root d))))))

(test first-child
  (it
    (with-nodes (a b c d)
      (is (eq b (first-child a))))))

(test last-child
  (it
    (with-nodes (a b c d)
      (is (eq d (last-child a))))))

(test sibling
  (it
    (with-nodes (a b c d e f)
      (is (equal `(,b ,c ,e ,f) (sibling d))))))

(test index
  (it
    (with-nodes (a b c d e f)
      (is (= 2 (index d)))
      (is (= 0 (index a))))))

(test previous-sibling
  (it
    (with-nodes (a b c d e f)
      (is (eq c (previous-sibling d)))
      (is (eq nil (previous-sibling a)))
      (is (eq nil (previous-sibling b))))))

(test next-sibling
  (it
    (with-nodes (a b c d e f)
      (is (eq e (next-sibling d)))
      (is (eq nil (next-sibling a)))
      (is (eq nil (next-sibling f))))))

(test preceding
  (it
    (with-nodes (a (b c d) (e f g))
      (is (equal nil (preceding a)))
      (is (equal a (preceding b)))
      (is (equal b (preceding c)))
      (is (equal c (preceding d)))
      (is (equal d (preceding e)))
      (is (equal e (preceding f))))))

(test following
  (it
    (with-nodes (a (b c d) (e f g))
      (is (equal b (following a)))
      (is (equal c (following b)))
      (is (equal d (following c)))
      (is (equal e (following d)))
      (is (equal f (following e)))
      (is (equal g (following f)))
      (is (equal nil (following g))))))
