(in-package :component)

(defun diff (a b)
  (check-type a html:element)
  (check-type b html:element)
  (diff-node a b))

;; (diff (html:h1) (html:h3))
;; (diff (html:h1 (html:h2) (html:h4)) (html:h1 (html:h3)))
;; (diff (html:h1 "foo") (html:h1 "bar"))

(defvar *diff-path* nil)

(defgeneric diff-node (a b)
  (:method ((a html:element) (b html:element))
    (if (equal (dom:tag-name a) (dom:tag-name b))
        (when (or (dom:children a) (dom:children b))
          (diff-children a b))
        `((:replace ,*diff-path* ,a ,b))))
  (:method ((a html:text) (b html:text))
    (unless (equal (dom:data a) (dom:data b))
      `((:replace ,*diff-path* ,a ,b))))
  (:method (a b)
    `((:replace ,*diff-path* ,a ,b))))

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
                  ((and a-child (not b-child)) `((:remove ,(append *diff-path* `(,index)) ,a-child)))
                  ((and b-child (not a-child)) `((:insert ,*diff-path* ,a ,index ,b-child)))
                  (t (let ((*diff-path* (append *diff-path* `(,index))))
                       (diff-node a-child b-child))))))))

;; (diff-children (html:p (html:h1) (html:h2)) (html:p (html:h3) (html:h4) (html:h5)))
