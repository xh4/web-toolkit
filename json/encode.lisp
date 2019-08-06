(in-package :json)

(defmethod cl-json:encode-json ((object object) &optional stream)
  (let ((pairs (slot-value object 'pairs)))
    (cl-json:encode-json pairs stream)))

(defun encode (value &optional target &key)
  (typecase target
    (null (cl-json:encode-json-to-string value))
    (stream (cl-json:encode-json value target))
    ((or string pathname)
     (with-open-file (stream target
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (cl-json:encode-json value stream)))
    (t (error "unknown target"))))

(defun encode-json (&rest args)
  (apply #'encode args))
