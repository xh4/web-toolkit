(in-package :component)

(defun diff (a b)
  (check-type a html:element)
  (check-type b html:element)
  (diff-node a b))

;; (diff (html:h1) (html:h3))
;; (diff (html:h1 (html:h2) (html:h4)) (html:h1 (html:h3)))
;; (diff (html:h1 "foo") (html:h1 "bar"))
;; (diff (html:h1 :class "aaa" "foo") (html:h1 :class "bbb" "bar"))

(defvar *diff-path* nil)

(defgeneric diff-node (a b)
  (:method ((a html:element) (b html:element))
    (if (equal (dom:tag-name a) (dom:tag-name b))
        (let ((a-attributes (slot-value a 'dom::attributes))
              (b-attributes (slot-value b 'dom::attributes)))
          (let ((attributes '()))
            (loop for (a-name . a-value) in a-attributes
               for (b-name . b-value) = (assoc a-name b-attributes :test 'equal)
               do (if b-name
                      (unless (equal a-value b-value)
                        (push `(,b-name . ,b-value) attributes))
                      (push `(,a-name . nil) attributes)))
            (loop for (b-name . b-value) in b-attributes
               for (a-name . nil) = (assoc b-name a-attributes :test 'equal)
               unless a-name
               do (push `(,b-name . ,b-value) attributes))
            (append (when attributes
                      `((:update ,*diff-path* ,a ,attributes)))
                    (when (or (dom:children a) (dom:children b))
                      (diff-children a b)))))
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
