(in-package :reactive)

(defmethod (setf slot-value-using-class) :around (value (class reactive-class) object slot)
  (typecase slot
    (symbol slot)
    (slot-definition (setf slot (slot-definition-name slot))))
  (when (or (eq slot 'dependency) (eq slot 'propagation))
    (return-from slot-value-using-class (call-next-method)))
  ;; (when *propagation-p*
  ;;   (format t "Set slot ~A of ~A (with propagation)~%" slot object))
  (when *propagation-p*
    (let ((current-value (slot-value object slot)))
      (unless (eq value current-value)
        (let ((record (make-instance 'record
                                     :object object
                                     :slot slot
                                     :previous-value current-value
                                     :current-value value
                                     :previous *record*)))
          (loop for o in (object-propagation object)
             do (if *local-propagation-p*
                    (push (cons o record) *local-propagation*)
                    (push (cons o record) *global-propagation*)))))))
  (prog1
      (call-next-method)
    (when (and *propagation-p*
               (not *local-propagation-p*))
      (propagate-all))))
