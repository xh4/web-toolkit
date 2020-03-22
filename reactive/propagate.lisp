(in-package :reactive)

(defun propagate-0 (object record)
  (labels ((detect-cycle (object record)
             (or (eq object (record-object record))
                 (and (record-previous record)
                      (detect-cycle object (record-previous record))))))
    (let ((*local-propagation* nil)
          (*local-propagation-p* t))
      (let ((*record* record))
        ;; cycle
        (unless (detect-cycle object record)
          (react object (record-object record))))
      *local-propagation*)))

(defun propagate-1 ()
  (when-let ((propagation (pop *global-propagation*)))
    (destructuring-bind (object . record) propagation
      (when object
        (let ((local-propagation (propagate-0 object record)))
          (loop for p in local-propagation
               do (push p *global-propagation*)))))
    propagation))

(defun propagate-all ()
  (loop while (propagate-1)))
