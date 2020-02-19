(in-package :uri)

(defun uri-query-alist (query-string)
  (let ((pairs (split-sequence #\& query-string)))
    (loop for pair in pairs
       for (name value) = (split-sequence #\= pair)
       for name-decoded = (percent-decode name)
       for value-decoded = (percent-decode value)
       collect (cons name-decoded (when value value-decoded)))))

(defun uri-query-hash-table (query-string)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (name . value) in (uri-query-alist query-string)
       do (setf (gethash name table) value))
    table))
