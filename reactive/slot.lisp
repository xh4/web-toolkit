(in-package :reactive)

(defmethod slot-value-using-class :around ((class reactive-class) object slot)
  (call-next-method))

(defmethod (setf slot-value-using-class) :around (value (class reactive-class) object slot)
  ;; (when *propagation-p*
  ;;   (typecase slot
  ;;     (symbol slot)
  ;;     (slot-definition (setf slot (slot-definition-name slot))))
  ;;   (format t "Set slot ~A of ~A (with propagation)~%" slot object))
  (when *propagation-p*
    (typecase slot
      (symbol slot)
      (slot-definition (setf slot (slot-definition-name slot))))
    (unless (member slot '(dependency propagation))
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
                      (push (cons o record) *global-propagation*))))))))
  (prog1
      (call-next-method)
    (when (and *propagation-p*
               (not *local-propagation-p*))
      (propagate-all))))
