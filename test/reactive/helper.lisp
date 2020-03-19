(in-package :reactive-test)

(defmacro variable (name)
  `(utility::variable ,name))
