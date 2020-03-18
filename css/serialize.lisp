(in-package :css)

(defmacro define-serialize-method ((object stream) &body body)
  `(defmethod serialize (,object &optional ,stream)
     (let ((string-stream-p (null ,stream)))
       (when string-stream-p (setf ,stream (make-string-output-stream)))
       ,@body
       (when string-stream-p
         (get-output-stream-string ,stream)))))

(defgeneric serialize (object &optional stream))
