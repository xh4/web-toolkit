(in-package :html)

(defclass constructor () ())

(defmethod print-object ((constructor constructor) stream)
  (print-unreadable-object (constructor stream :type t)))

(defgeneric construct (constructor &rest arguments))
