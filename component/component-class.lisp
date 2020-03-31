(in-package :component)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass component-class (reactive-class) ()))

(defmethod shared-initialize :after ((class component-class) slot-names
                                     &key &allow-other-keys)
  (declare (ignore slot-names))
  (loop for object in (object-propagation class)
     when (typep object 'component)
     do (incf (slot-value object 'version))))

(defmethod (setf slot-value-using-class) :around (value (class component-class) component slot)
  (declare (ignore value))
  (typecase slot
    (symbol slot)
    (slot-definition (setf slot (slot-definition-name slot))))
  (if *component-initializing*
      (without-propagation (call-next-method))
      (if (eq 'version slot)
          (call-next-method)
          (prog1
              (without-propagation (call-next-method))
            (when (find slot (class-direct-slots class)
                        :key 'slot-definition-name)
              (incf (slot-value component 'version)))))))
