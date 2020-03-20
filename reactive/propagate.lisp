(in-package :reactive)

(defun propagate-0 (object record)
  (let ((*local-propagation* nil)
        (*local-propagation-p* t))
    (let ((*record* record))
      ;; cycle
      (unless (eq object (record-object record))
        (react object (record-object record))))
    *local-propagation*))

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

(defmacro without-propagation (&body body)
  `(let ((*propagation-p* nil))
     ,@body))

(defmacro with-propagation (&body body)
  `(let ((*propagation-p* t))
     ,@body))

(define-reactive-class myclass ()
  ((a :initform 1)))

(let ((o1 (make-instance 'myclass))
      (o2 (make-instance 'myclass))
      (o3 (make-instance 'myclass)))
  (add-dependency o2 o1)
  (add-dependency o3 o2)
  (setf (slot-value o1 'a) 2))
