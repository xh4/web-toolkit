(in-package :reactive)

(defclass reactive-method (standard-method reactive-object) ())

;; (defmethod shared-initialize :after ((method reactive-method) slot-names &rest initargs &key &allow-other-keys)
;;   (loop for class in (closer-mop:method-specializers method)
;;      when (typep class 'reactive-class)
;;      do (update class t)))
