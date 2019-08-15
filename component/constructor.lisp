(in-package :component)

(defmacro define-constructor (name bindings &body body)
  (defmethod initialize-instance :after ))
