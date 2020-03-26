(in-package :component)

(defun render-all (node)
  (let ((table (make-hash-table)))
    (labels ((render-1 (node table)
               (typecase node
                 (html:text node)
                 (string (html:text node))
                 (component
                  (let ((root (render node)))
                    (setf (gethash node table) root)
                    (loop for i from 0
                       for child in (dom:children root)
                       do (setf (nth i (dom:children root)) (render-1 child table))
                       finally (return root))))
                 (html:element
                  (loop for i from 0
                     for child in (dom:children node)
                     do (setf (nth i (dom:children node)) (render-1 child table))
                     finally (return node))))))
      (values (render-1 node table) table))))
