(in-package :javascript)

(defun substring (string start &optional end)
  (let ((end (if (> end (length string))
                 (length string)
               end)))
    (subseq string start end)))

(defun char-at (string index)
  (when (< index (length string))
      (char string index)))

