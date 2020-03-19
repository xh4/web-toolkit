(in-package :reactive)

(defclass reactive-class (standard-class reactive-object) ())

(defmethod closer-mop:validate-superclass
    ((class reactive-class) (super-class standard-class)) t)
