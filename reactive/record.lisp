(in-package :reactive)

(defclass record ()
  ((object
    :initarg :object
    :initform nil
    :accessor record-object)
   (slot
    :initarg :slot
    :initform nil
    :accessor record-slot)
   (current-value
    :initarg :current-value
    :initform nil
    :accessor record-current-value)
   (previous-value
    :initarg :previous-value
    :initform nil
    :accessor record-previous-value)
   (previous
    :initarg :previous
    :initform nil
    :accessor record-previous)))
