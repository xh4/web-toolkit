(in-package :html)

(defun nth* (i list)
  (if (< i 0)
      (nth (+ (length list) i ) list)
    (nth i list)))

(defun (setf nth*) (value i list)
  (if (< i 0)
      (setf (nth (+ (length list) i ) list) value)
    (setf (nth i list) value)))

