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
    :initform 'identity)))

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
