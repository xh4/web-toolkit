(in-package :dom)

(defclass node-iterator ()
  ((root
    :initarg :root
    :initform nil
    :reader root)
   (reference
    :initarg :reference
    :initform nil
    :reader reference)
   (pointer-before-reference
    :initarg :pointer-before-reference
    :initform t
    :reader pointer-before-reference)
   (filter
    :initarg :filter
    :initform 'identity
    :reader filter)))

(defun create-node-iterator (root &optional (filter 'identity))
  (make-instance 'node-iterator
                 :root root
                 :reference root
                 :pointer-before-reference t
                 :filter filter))

(defun traverse (iterator direction)
  (let ((node (reference iterator))
        (before-node (pointer-before-reference iterator))
        (filter (slot-value iterator 'filter)))
    (loop
       (case direction
         (:next
          (cond
            ((not before-node)
             (if-let ((next (following node)))
               (setf node next)
               (return-from traverse nil)))
            (before-node
             (setf before-node nil))))
         (:previous
          (cond
            (before-node
             (if-let ((previous (preceding node)))
               (setf node previous)
               (return-from traverse nil)))
            ((not before-node)
             (setf before-node t)))))
       (when (funcall filter node)
         (return)))
    (setf (slot-value iterator 'reference) node)
    (setf (slot-value iterator 'pointer-before-reference) before-node)
    node))

(defgeneric next-node (node-iterator)
  (:method ((node-iterator node-iterator))
    (traverse node-iterator :next)))

(defgeneric previous-node (node-iterator)
  (:method ((node-iterator node-iterator))
    (traverse node-iterator :previous)))

(defclass tree-walker ()
  ((root
    :initarg :root
    :initform nil
    :reader root)
   (current
    :initarg :current
    :initform nil
    :accessor current)
   (filter
    :initarg :filter
    :initform 'identity
    :reader filter)))

(defgeneric current-node (tree-walker)
  (:method ((tree-walker tree-walker))
    (slot-value tree-walker 'current)))

(defmethod parent-node (tree-walker)
  (let ((node (current tree-walker)))
    (loop while (and node (not (eq node (root tree-walker))))
       do (setf node (parent node))
         (when (and node (funcall (filter tree-walker) node))
           (setf (current tree-walker) node)
           (return-from parent-node node)))))

(defun traverse-children (walker type)
  (let ((node (current walker)))
    (case type
      (:first (setf node (first-child node)))
      (:last-first (setf node (last-child node))))
    (loop while node
       for result = (funcall (filter walker node))
       do (if result
              (progn
                (setf (current walker) node)
                (return-from traverse-children node))
              (progn
                (let ((child (case type
                               (:first (first-child node))
                               (:last (last-child node)))))
                  (when child
                    (setf node child)
                    (continue)))))
         (loop while node
            do (let ((sibling (case type
                                (:first (next-sibling node))
                                (:last (previous-sibling node)))))
                 (when sibling
                   (setf node sibling) (return))
                 (let ((parent (parent node)))
                   (when (or (null parent)
                             (eq (root walker) parent)
                             (eq (current walker) parent))
                     (return-from traverse-children nil))
                   (setf node parent))) ))))

(defmethod first-child (tree-walker)
  (traverse-children tree-walker :first))

(defmethod last-child (tree-walker)
  (traverse-children tree-walker :last))

(defun traverse-sibling (walker type)
  (let ((node (current walker)))
    (when (eq (root walker) node)
      (return-from traverse-sibling nil))
    (loop with result
       do (let ((sibling (case type
                           (:next (next-sibling node))
                           (:previous (previous-sibling node)))))
            (loop while sibling
               do
                 (setf node sibling)
                 (setf result (funcall (filter walker) node))
                 (when result
                   (setf (current walker) node)
                   (return-from traverse-sibling node))
                 (setf sibling (case type
                                 (:next (first-child node))
                                 (:previous (last-child node))))
                 (when (or (not result) (null sibling))
                   (setf sibling (case type
                                   (:next (next-sibling node))
                                   (:previous (previous-sibling node))))))
            (setf node (parent node))
            (when (or (null node) (eq (root walker) node))
              (return-from traverse-sibling nil))
            (when result
              (return-from traverse-sibling nil))))))

(defmethod previous-sibling ((walker tree-walker))
  (traverse-sibling walker :previous))

(defmethod next-sibling ((walker tree-walker))
  (traverse-sibling walker :next))

(defmethod previous-node ((walker tree-walker))
  (let ((node (current walker)))
    (loop with result
       while (not (eq node (root walker)))
       do
         (let ((sibling (previous-sibling node)))
           (loop while sibling
              do
                (setf sibling node)
                (setf result (funcall (filter walker) node))
                (loop while (and result (children node))
                   do
                     (setf node (last-child node))
                     (setf result (funcall (filter walker) node)))
                (when result
                  (setf (current walker) node)
                  (return-from previous-node node))
                (setf sibling (previous-sibling node))))
         (when (or (eq node (root walker))
                   (null (parent node)))
           (return-from previous-node nil))
         (setf node (parent node))
         (when result
           (setf (current walker) node)
           (return-from previous-node node)))
    (return-from previous-node nil)))

(defmethod next-node ((walker tree-walker))
  (let ((node (current walker))
        (result t))
    (loop
       (loop while (and result (children node))
          do
            (setf node (first-child node))
            (setf result (funcall (filter walker) node))
            (when result
              (setf (current walker) node)
              (return-from next-node node)))
       (let ((sibling nil)
             (temporary node))
         (loop while temporary
            do
              (when (eq temporary (root walker))
                (return-from next-node nil))
              (setf sibling (next-sibling temporary))
              (when sibling
                (setf node sibling)
                (return))
              (setf temporary (parent temporary))))
       (setf result (funcall (filter walker) node))
       (when result
         (setf (current walker) node)
         (return-from next-node node)))))

(defun create-tree-walker (root &optional (filter 'identity))
  (make-instance 'tree-walker
                 :root root
                 :current root
                 :filter filter))
