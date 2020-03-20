(in-package :reactive)

(defclass reactive-object ()
  ((dependency
    :initarg :dependency
    :initform nil)
   (propagation
    :initarg :propagation
    :initform nil)))

(defmethod object-dependency ((object reactive-object))
  (loop for pointer in (slot-value object 'dependency)
     for object = (weak-pointer-value pointer)
     when object
     collect object))

(defmethod object-propagation ((object reactive-object))
  (loop for pointer in (slot-value object 'propagation)
     for object = (weak-pointer-value pointer)
     when object
     collect object))

(defmethod shared-initialize :around ((object reactive-object) slot-names &rest initargs &key &allow-other-keys)
  (let ((object (call-next-method)))
    (finalize
     object
     (lambda ()
       (loop for pointer in (slot-value object 'dependency)
          for o = (weak-pointer-value pointer)
          when o
          do (setf (slot-value o 'propagation)
                   (remove object (slot-value o 'propagation)
                           :key #'weak-pointer-value)))
       (loop for pointer in (slot-value object 'propagation)
          for o = (weak-pointer-value pointer)
          when o
          do (setf (slot-value o 'dependency)
                   (remove object (slot-value o 'dependency)
                           :key #'weak-pointer-value)))
       object))
    object))

(defun add-dependency (object-1 object-2)
  (when (and object-1 object-2)
    (unless (find object-2 (object-dependency object-1))
      (push (make-weak-pointer object-1) (slot-value object-2 'propagation))
      (push (make-weak-pointer object-2) (slot-value object-1 'dependency)))))

(defmethod shared-initialize :after ((object reactive-object) slot-names &rest initargs &key &allow-other-keys)
  (let ((class (class-of object)))
    (when (typep class 'reactive-class)
      (add-dependency object class))))
