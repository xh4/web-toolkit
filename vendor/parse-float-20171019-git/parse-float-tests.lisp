;;;; parse-float-tests.lisp

(defpackage :parse-float-tests
  (:use :common-lisp :lisp-unit :parse-float))

(in-package :parse-float-tests)

(defvar *test-values*
  '(0 1 -1 1/10 -1/10 10 -10 12 -12 1/8 -1/8 123 -123 1/800 -1/800 1234 -1234 1/1600 -1/1600 314159/100000 -314159/100000))

(defun as-fixed-format-floating-point (value)
  (format nil "~,8F" value))

(defun as-fixed-format-floating-point-with-plus (value)
  (when (>= value 0)
    (format nil "+~,8F" value)))

(defun as-fixed-format-floating-point-without-leading-zero (value)
  (cond
    ((< 0 value 1)
     (subseq (as-fixed-format-floating-point value) 1))
    ((< -1 value 0)
     (format nil "-~A" (subseq (as-fixed-format-floating-point (- value)) 1)))
    (t nil)))

(defun as-exponential-floating-point (value)
  (format nil "~,,,,,,'eE" value))

(defun make-test-strings (value)
  (loop for printer in (list #'as-fixed-format-floating-point
			     #'as-fixed-format-floating-point-with-plus
			     #'as-fixed-format-floating-point-without-leading-zero
			     #'as-exponential-floating-point)
     for string = (funcall printer value)
     when string
     collect string))

(defun print-test-strings (value)
  (format t "Test value: ~A~%" value)
  (loop for string in (make-test-strings value)
     do (format t "          : ~A~%" string)))

(defun print-all-test-strings ()
  (loop for value in *test-values*
     do (print-test-strings value)))

(define-test parse-float
  (dolist (value *test-values*)
    (dolist (string (make-test-strings value))
      (assert-equal value (parse-float string :type 'number))
      (assert-equal (coerce value 'single-float) (parse-float string :type 'single-float))
      (assert-equal (coerce value 'double-float) (parse-float string :type 'double-float)))))
