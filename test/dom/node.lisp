(in-package :dom-test)

(in-suite :dom-test)

(test node-types
  (it
    (is (equal dom:element-node (dom:node-type (make-instance 'dom:element))))
    (is (equal dom:attribute-node (dom:node-type (make-instance 'dom:attr))))
    (is (equal dom:text-node (dom:node-type (make-instance 'dom:text))))
    (is (equal dom:document-node (dom:node-type (make-instance 'dom:document))))
    (is (equal dom:comment-node (dom:node-type (make-instance 'dom:comment))))
    (is (equal dom:document-fragment-node (dom:node-type (make-instance 'dom:document-fragment))))
    (is (equal dom:cdata-section-node (dom:node-type (make-instance 'dom:cdata-section))))
    (is (equal dom:processing-instruction-node (dom:node-type (make-instance 'dom:processing-instruction))))))

(test node-name
  (it
    (is (equal "#text" (dom:node-name (make-instance 'dom:text))))
    (is (equal "#cdata-section" (dom:node-name (make-instance 'dom:cdata-section))))
    (is (equal "#document-fragment" (dom:node-name (make-instance 'dom:document-fragment))))
    (is (equal "#comment" (dom:node-name (make-instance 'dom:comment))))))

(test owner-document
  (it
    (is (null (dom:owner-document (make-instance 'dom:document))))))

(test append-child
  (it
    (let ((parent (make-instance 'dom:element)))
      (let ((child-1 (make-instance 'dom:element))
            (child-2 (make-instance 'dom:element))
            (child-3 (make-instance 'dom:element))
            (child-4 (make-instance 'dom:element))
            (child-5 (make-instance 'dom:element))
            (child-6 (make-instance 'dom:element)))
        (is (= 0 (dom:length (dom:child-nodes parent))))
        (is (null (dom:first-child parent)))
        (is (null (dom:last-child parent)))
        (is (null (dom:previous-sibling child-1)))
        (is (null (dom:next-sibling child-1)))

        (dom:append-child parent child-1)
        (is (= 1 (dom:length (dom:child-nodes parent))))
        (is (eq child-1 (dom:item (dom:child-nodes parent) 0)))
        (is (eq child-1 (dom:first-child parent)))
        (is (eq child-1 (dom:last-child parent)))
        (is (null (dom:previous-sibling child-1)))
        (is (null (dom:next-sibling child-1)))

        (dom:append-child parent child-2)
        (is (= 2 (dom:length (dom:child-nodes parent))))
        (is (eq child-2 (dom:item (dom:child-nodes parent) 1)))
        (is (eq child-1 (dom:first-child parent)))
        (is (eq child-2 (dom:last-child parent)))
        (is (eq child-1 (dom:previous-sibling child-2)))
        (is (eq child-2 (dom:next-sibling child-1)))

        (dom:append-child parent child-3)
        (is (= 3 (dom:length (dom:child-nodes parent))))
        (is (eq child-3 (dom:item (dom:child-nodes parent) 2)))
        (is (eq child-1 (dom:first-child parent)))
        (is (eq child-3 (dom:last-child parent)))
        (is (eq child-1 (dom:previous-sibling child-2)))
        (is (eq child-2 (dom:next-sibling child-1)))
        (is (eq child-3 (dom:next-sibling child-2)))
        (is (eq child-2 (dom:previous-sibling child-3)))

        (dom:append-child parent child-4)
        (is (= 4 (dom:length (dom:child-nodes parent))))
        (is (eq child-4 (dom:item (dom:child-nodes parent) 3)))

        (dom:append-child parent child-5)
        (is (= 5 (dom:length (dom:child-nodes parent))))
        (is (eq child-5 (dom:item (dom:child-nodes parent) 4)))

        (dom:append-child parent child-6)
        (is (= 6 (dom:length (dom:child-nodes parent))))
        (is (eq child-6 (dom:item (dom:child-nodes parent) 5)))

        (is (eq parent (dom:parent-node child-1)))
        (is (eq child-1 (dom:first-child parent)))
        (is (eq child-6 (dom:last-child parent)))))))

(test first-child
  (it
    (with-nodes (a b c d)
      (is (eq b (first-child a))))))

(test last-child
  (it
    (with-nodes (a b c d)
      (is (eq d (last-child a))))))

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
      (is (equal nil (dom::preceding a)))
      (is (equal a (dom::preceding b)))
      (is (equal b (dom::preceding c)))
      (is (equal c (dom::preceding d)))
      (is (equal d (dom::preceding e)))
      (is (equal e (dom::preceding f))))))

(test following
  (it
    (with-nodes (a (b c d) (e f g))
      (is (equal b (dom::following a)))
      (is (equal c (dom::following b)))
      (is (equal d (dom::following c)))
      (is (equal e (dom::following d)))
      (is (equal f (dom::following e)))
      (is (equal g (dom::following f)))
      (is (equal nil (dom::following g))))))

(test insert-before
  (it
    (with-nodes (a)
      (with-nodes (b)
        (insert-before a b nil)
        (is (equal 1 (dom:length (dom:child-nodes a))))
        (is (eq b (dom:first-child a)))
        (is (eq a (dom:parent-node b)))))

    (with-nodes (a b d)
      (with-nodes (c)
        (insert-before a c d)
        (is (equal 3 (dom:length (dom:child-nodes a))))
        (is (eq c (dom:item (dom:child-nodes a) 1)))
        (is (eq a (dom:parent-node c)))))

    (with-nodes (a c d)
      (with-nodes (b)
        (insert-before a b c)
        (is (equal 3 (dom:length (dom:child-nodes a))))
        (is (eq b (dom:first-child a)))
        (is (eq a (dom:parent-node b)))))))

(test node-value
  (it
    (is (equal "foo" (dom:node-value (make-instance 'dom:text :data "foo"))))
    (is (equal "foo" (dom:node-value (make-instance 'dom:comment :data "foo"))))
    (is (equal "foo" (dom:node-value (make-instance 'dom:attr :value "foo"))))
    (is (null (dom:node-value (make-instance 'dom:element))))
    (is (null (dom:node-value (make-instance 'dom:document))))))

(test text-content
  (it
    (is (equal "foo" (dom:text-content (make-instance 'dom:text :data "foo"))))
    (is (equal "foo" (dom:text-content (make-instance 'dom:comment :data "foo"))))
    (is (equal "foo" (dom:text-content (make-instance 'dom:attr :value "foo"))))
    ;; TODO: for element and text-fragment
    (is (null (dom:node-value (make-instance 'dom:document))))))
