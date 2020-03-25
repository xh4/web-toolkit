(in-package :component)

(defun render-all (node)
  (typecase node
    (html:text node)
    (string (html:text node))
    (component
     (setf node (render node))
     (loop for i from 0
        for child in (dom:children node)
        do (setf (nth i (dom:children node)) (render-all child))
        finally (return node)))
    (html:element
     (loop for i from 0
        for child in (dom:children node)
        do (setf (nth i (dom:children node)) (render-all child))
        finally (return node)))))
