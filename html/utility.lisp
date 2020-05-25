(in-package :html)

(defvar +replacement-character+
  #+lispworks #\replacement-character
  #-lispworks #\replacement_character)

(defun nth* (i list)
  (if (< i 0)
      (nth (+ (length list) i ) list)
    (nth i list)))

(defun (setf nth*) (value i list)
  (if (< i 0)
      (setf (nth (+ (length list) i ) list) value)
    (setf (nth i list) value)))

(defun make-adjustable-string (&optional (s ""))
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))