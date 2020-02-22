(in-package :dom-test)

(in-suite :dom-test)

(test node-iterator/next-node
  (it
    (with-nodes (a (b c d) (e f g))
      (let ((iterator (create-node-iterator a)))
        (is (equal a (next-node iterator)))
        (is (equal b (next-node iterator)))
        (is (equal c (next-node iterator)))
        (is (equal d (next-node iterator)))
        (is (equal e (next-node iterator)))
        (is (equal f (next-node iterator)))
        (is (equal g (next-node iterator)))
        (is (equal nil (next-node iterator)))
        (is (equal nil (next-node iterator)))))))


(test node-iterator/previous-node
  (it
    (with-nodes (a (b c d) (e f g))
      (let ((iterator (create-node-iterator g)))
        (is (equal f (previous-node iterator)))
        (is (equal e (previous-node iterator)))
        (is (equal d (previous-node iterator)))
        (is (equal c (previous-node iterator)))
        (is (equal b (previous-node iterator)))
        (is (equal a (previous-node iterator)))
        (is (equal nil (previous-node iterator)))
        (is (equal nil (previous-node iterator)))))))

(test node-iterator
  (it
    (with-nodes (a (b c d) (e f g))
      (let ((iterator (create-node-iterator d)))
        (is (equal d (next-node iterator)))
        (is (equal e (next-node iterator)))
        (is (equal e (previous-node iterator)))
        (is (equal d (previous-node iterator)))
        (is (equal d (next-node iterator)))))))
