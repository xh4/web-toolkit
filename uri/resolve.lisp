(in-package :uri)

(defun normalize-scheme (scheme)
  (string-downcase scheme))

(defun normalize-host (host)
  (string-downcase host))

(defun normalize-path-segment (segment)
  )
