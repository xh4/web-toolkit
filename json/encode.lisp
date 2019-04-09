(in-package :wt.json)

(defmethod json:encode-json ((object fluid-object) &optional stream)
  (let ((slots-hash-table (slot-value object '%slots)))
    (json:encode-json slots-hash-table stream)))

(defun encode-json (value &optional target &key)
  (typecase target
    (null (json:encode-json-to-string value))
    (stream (json:encode-json value target))
    ((or string pathname)
     (with-open-file (stream target
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (json:encode-json value stream)))
    (t (error "unknown target"))))
