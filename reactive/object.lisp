(in-package :reactive)

(defclass reactive-object ()
  ((dependency
    :initarg :dependency
    :initform (make-weak-hash-table :test 'eq
                                    :weakness :key
                                    :weakness-matters t))
   (propagation
    :initarg :propagation
    :initform (make-weak-hash-table :test 'eq
                                    :weakness :key
                                    :weakness-matters t))))

(defmethod object-dependency ((object reactive-object))
  (with-slots (dependency) object
    (hash-table-keys dependency)))

(defmethod object-propagation ((object reactive-object))
  (with-slots (propagation) object
    (hash-table-keys propagation)))

(defmethod shared-initialize :after ((object reactive-object) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (let ((class (class-of object)))
    (when (typep class 'reactive-class)
      (add-dependency object class))))

(defun add-dependency (object-1 object-2)
  (when (and object-1 object-2)
    (with-slots (dependency) object-1
      (setf (gethash object-2 dependency) nil))
    (with-slots (propagation) object-2
      (setf (gethash object-1 propagation) nil))))

(defun remove-dependency (object-1 object-2)
  (when (and object-1 object-2)
    (with-slots (dependency) object-1
      (remhash object-2 dependency))
    (with-slots (propagation) object-2
      (remhash object-1 propagation))))
