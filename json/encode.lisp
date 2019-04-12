(in-package :wt.json)

(defmethod json:encode-json ((object fluid-object) &optional stream)
  (let ((slots-hash-table (slot-value object '%slots)))
    (cl-json:encode-json slots-hash-table stream)))

(defun encode-json (value &optional target &key)
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
