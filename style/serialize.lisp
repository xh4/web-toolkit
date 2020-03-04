(in-package :style)

(defgeneric serialize (object &optional stream))

(defmethod serialize ((declaration declaration) &optional (stream *standard-output*))
  (let ((name (declaration-name declaration))
        (value (declaration-value declaration)))
    (format stream "~(~A~): " name)
    (serialize value stream)))

(defmethod serialize ((dimension dimension) &optional (stream *standard-output*))
  (let ((number (dimension-number dimension))
        (unit (dimension-unit dimension)))
    (format stream "~A~(~A~)" number (symbol-name unit))))
