(in-package :json)

(defgeneric value (object)
  (:method (object) object))
