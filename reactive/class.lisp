(in-package :reactive)

(defclass reactive-class (standard-class reactive-object) ())

(defmethod validate-superclass
    ((class reactive-class) (super-class standard-class)) t)

(defmacro define-reactive-class (class-name superclasses slots &rest options)
  (unless (find 'reactive-object superclasses)
    (appendf superclasses '(reactive-object)))
  (unless (find :metaclass options :key 'first)
    (rewrite-class-option options :metaclass reactive-class))
  #+lispworks
  (rewrite-class-option options :optimize-slot-access nil)
  `(defclass ,class-name ,superclasses ,slots ,@options))
