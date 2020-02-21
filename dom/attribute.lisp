(in-package :dom2)

(defclass attribute (node)
  ((name        :initarg :name          :reader name)
   (owner-element :initarg :owner-element :reader owner-element)
   (specified-p :initarg :specified-p   :reader specified)))

(defmethod print-object ((object attribute) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A=~S"
            (name object)
            (value object))))
