(in-package :component)

(defun compute-style-rules/1 (component &optional (rules (make-hash-table)))
  (unless (gethash component rules)
    (let* ((classes (compute-component-class component))
           (selector (format nil "~{.~A~}" classes))
           (rule (make-instance 'qualified-rule
                               :selectors (list selector)
                               :declarations (append
                                              (style:style-declarations
                                               (slot-value component 'style))
                                              (style:style-declarations
                                               (slot-value (component-root component) 'style))))))
      (setf (gethash component rules) rule))))

(defun compute-style-rules (component)
  (let ((rules (make-hash-table)))
    (labels ((walk (node)
               (typecase node
                 (component (compute-style-rules/1 node rules)
                            (mapcar #'walk (children node)))
                 (html:element (mapcar #'walk (children node))))))
      (walk component)
      (hash-table-values rules))))

;; (compute-style-rules (foo (html:h1 (bar)) (html:h2) (html:h3)))
