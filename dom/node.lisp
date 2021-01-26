(in-package :dom)

(defconstant element-node 1)
(defconstant attribute-node 2)
(defconstant text-node 3)
(defconstant cdata-section-node 4)
(defconstant entity-reference-node 5)
(defconstant entity-node 6)
(defconstant processing-instruction-node 7)
(defconstant comment-node 8)
(defconstant document-node 9)
(defconstant document-type-node 10)
(defconstant document-fragment-node 11)

(defclass node ()
  ((document
    :initarg :document
    :initform nil
    :reader owner-document)))

(defclass node-list ()
  ((nodes
    :initarg :nodes
    :initform nil)))

(defmethod print-object ((node-list node-list) stream)
  (print-unreadable-object (node-list stream :type t :identity t)
    (format stream "{~A}" (length node-list))))

(defgeneric length (object)
  (:method ((node-list node-list))
    (cl:length (slot-value node-list 'nodes))))

(defgeneric item (object index)
  (:method ((node-list node-list) (index integer))
    (if (>= index (length node-list))
        nil
        (aref (slot-value node-list 'nodes) index))))

(defun node-list-nodes (node-list)
  (coerce (slot-value node-list 'nodes) 'list))

(defgeneric node-type (node))

(defgeneric node-name (node))

(defgeneric owner-document (node))

(defgeneric parent-node (node)
  (:method ((node node))
    (slot-value node 'parent-node)))

(defmethod parent ((node node))
  (parent-node node))

(defgeneric parent-element (node))

(defgeneric has-child-nodes (node)
  (:method ((node node))
    (plusp (cl:length (slot-value node 'child-nodes)))))

(defgeneric child-nodes (node)
  (:method ((node node))
    (make-instance 'node-list :nodes (slot-value node 'child-nodes))))

(defmethod children ((node node))
  (slot-value (child-nodes node) 'nodes))

(defmethod first-child ((node node))
  (let ((child-nodes (slot-value node 'child-nodes)))
    (if (plusp (cl:length child-nodes))
        (aref child-nodes 0))))

(defmethod last-child ((node node))
  (let ((child-nodes (slot-value node 'child-nodes)))
    (if (plusp (cl:length child-nodes))
        (aref child-nodes (1- (cl:length child-nodes))))))

(defmethod sibling ((node node))
  (when-let ((parent (parent node)))
    (cl:remove node (slot-value parent 'child-nodes))))

(defmethod index ((node node))
  (if-let ((parent-node (parent-node node)))
    (or (position node (slot-value parent-node 'child-nodes)) 0)
    0))

(defmethod previous-sibling ((node node))
  (when-let ((parent-node (parent-node node)))
    (let ((index (index node)))
      (if (< 0 index)
          (aref (slot-value parent-node 'child-nodes) (1- index))))))

(defmethod next-sibling ((node node))
  (when-let ((parent-node (parent-node node)))
    (let ((index (index node)))
      (if (< index (1- (cl:length (slot-value parent-node 'child-nodes))))
          (aref (slot-value parent-node 'child-nodes) (1+ index))))))

(defmethod preceding ((node node))
  (let ((index (index node)))
    (labels ((down (node)
               (if-let ((last-child (last-child node)))
                 (down last-child)
                 node)))
      (if (= index 0)
          (parent node)
          (down (previous-sibling node))))))

(defmethod following ((node node))
  (labels ((up (node)
              (if-let ((parent (parent node)))
                (or (next-sibling parent)
                    (up parent)))))
    (if (has-child-nodes node)
        (aref (children node) 0)
        (or (next-sibling node)
            (up node)))))

(defgeneric node-value (node)
  (:method ((node node))
    nil))

(defgeneric text-content (node)
  (:method ((node node))
    nil))

(defgeneric normalize (node))

(defun clone (node &key document clone-children)
  (flet ((clone-1 (node)
           (let ((document (or document (slot-value node 'document))))
             (typecase node
               (element (make-instance 'element
                                       :document document
                                       :local-name (slot-value node 'local-name)
                                       :namespace (slot-value node 'namespace)
                                       :namespace-prefix (slot-value node 'prefix)
                                       :attributes (slot-value node 'attributes)))
               (document (make-instance 'document))
               (text (make-instance 'text :data (slot-value node 'data)))))))
    (let ((copy (clone-1 node)))
      (when clone-children
        (loop for child in (children node)
              do (append-child copy (clone-1 child))))
      copy)))

(defgeneric clone-node (node &optional deep)
  (:method ((node node) &optional deep)
   (clone node :clone-children t)))

(defgeneric contains (node other)
  (:method ((node node) (other node))
    ))

(defgeneric insert-before (parent node child)
  (:method ((parent node) (node node) (child node))
    (let ((j (index child)))
      (unless j
        (error "Child ~A not found" child))
      (setf (slot-value node 'parent-node) parent)
      (with-slots (child-nodes) parent
        (vector-push-extend nil child-nodes)
        (loop for i from (1- (cl:length child-nodes)) downto (1+ j)
           do (setf (aref child-nodes i) (aref child-nodes (1- i)))
           finally (setf (aref child-nodes j) node))))
    node)
  (:method ((parent node) (node node) (child null))
    (setf (slot-value node 'parent-node) parent)
    (with-slots (child-nodes) parent
      (vector-push-extend nil child-nodes)
      (loop for i from (1- (cl:length child-nodes)) downto 1
         do (setf (aref child-nodes i) (aref child-nodes (1- i)))
         finally (setf (aref child-nodes 0) node)))
    node))

(defgeneric append-child (node child)
  (:method ((node node) (child node))
    (with-slots (child-nodes) node
      (vector-push-extend child child-nodes)
      (setf (slot-value child 'parent-node) node))
    child)
  (:method ((node node) (child null))))

(defgeneric replace-child (node child))

(defgeneric remove-child (node child))
