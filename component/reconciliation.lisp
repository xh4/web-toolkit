(in-package :component)

(defun diff (a b)
  (check-type a html:element)
  (check-type b html:element)
  (diff-node a b))

;; (diff (html:h1) (html:h3))
;; (diff (html:h1 (html:h2) (html:h4)) (html:h1 (html:h3)))
;; (diff (html:h1 "foo") (html:h1 "bar"))

(defgeneric diff-node (a b)
  (:method ((a html:element) (b html:element))
    (if (equal (dom:tag-name a) (dom:tag-name b))
        (when (or (dom:children a) (dom:children b))
          (diff-children a b))
        `((:replace ,a ,b))))
  (:method ((a html:text) (b html:text))
    (unless (equal (dom:data a) (dom:data b))
      `((:replace ,a ,b))))
  (:method (a b)
    `((:replace ,a ,b))))

(defun diff-children (a b)
  (let ((a-children (dom:children a))
        (b-children (dom:children b)))
    (let ((length (max (length a-children) (length b-children))))
      (appendf a-children (loop repeat (- length (length a-children)) collect nil))
      (appendf b-children (loop repeat (- length (length b-children)) collect nil))
      (loop for index from 0
         for a-child in a-children
         for b-child in b-children
         append (cond
                  ((and a-child (not b-child)) `((:remove ,a-child)))
                  ((and b-child (not a-child)) `((:insert ,a ,index ,b-child)))
                  (t (diff-node a-child b-child)))))))

;; (diff-children (html:p (html:h1) (html:h2)) (html:p (html:h3) (html:h4) (html:h5)))

(defun element-path (root node)
  (labels ((walk (root node path)
             (if (eq root node)
                 (return-from element-path (reverse path))
                 (when (typep root 'dom::parent-node)
                   (loop for child in (dom:children root)
                      for index from 0
                      do (walk child node (cons index path)))))))
    (walk root node '())))

;; (let ((node (html:a)))
;;   (element-path (html:p (html:h1 (html:h2 (html:h3) node))) node))
