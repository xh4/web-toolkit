(in-package :component)

(defun diff (a b)
  (check-type a html:element)
  (check-type b html:element)
  (diff-node a b))

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
                      (unless (if (equal a-name "class")
                                  (class-equal a-value b-value)
                                  (equal a-value b-value))
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

(defun class-equal (a-class b-class)
  (let ((a-classes (split-sequence #\Space a-class))
        (b-classes (split-sequence #\Space b-class)))
    (equal (sort a-classes #'string<) (sort b-classes #'string<))))

(defun diff-children (a b)
  (let ((a-children (dom:children a))
        (b-children (dom:children b)))
    (loop for index from 0 to (max (length a-children) (length b-children))
       for a-child = (nth index a-children)
       for b-child = (nth index b-children)
       append (cond
                ((and a-child (not b-child))
                 `((:remove ,(append *diff-path* `(,index)) ,a-child)))
                ((and (not a-child) b-child)
                 `((:insert ,*diff-path* ,a ,index ,b-child)))
                (t (let ((*diff-path* (append *diff-path* `(,index))))
                     (diff-node a-child b-child)))))))
