(in-package :dom-test)

(defclass node (dom:node)
  ((name
    :initarg :name
    :initform nil)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream)
    (format stream "~S" (slot-value node 'name))))

(defmacro with-nodes ((&rest forms) &body body)
  (let ((nodes (flatten forms))
        (relations '()))
    (labels ((do-form (form)
               (cond
                 ((and (listp form) (symbolp (first form)))
                  (let ((parent (first form)))
                    (loop for form in (rest form)
                       do (cond
                            ((symbolp form)
                             (appendf relations `((,parent ,form))))
                            ((and (listp form) (symbolp (first form)))
                             (appendf relations `((,parent ,(first form))))
                             (do-form form)))))))))
      (do-form forms))
    `(let ,(loop for node in nodes
              collect `(,node (make-instance 'node :name ',node)))
       ,@(loop for (parent child) in relations
              collect `(append-child ,parent ,child))
       ,@body)))
