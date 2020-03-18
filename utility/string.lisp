(in-package :utility)

(defun string-prefix-p (prefix string)
  (when (>= (length string) (length prefix))
    (string= string prefix :start1 0 :end1 (length prefix))))

(defun string-suffix-p (suffix string)
  (when (>= (length string) (length suffix))
    (string= string suffix :start1 (- (length string) (length suffix)))))
