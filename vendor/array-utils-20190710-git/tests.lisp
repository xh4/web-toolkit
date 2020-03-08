#|
 This file is a part of array-utils
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:array-utils-test
  (:use #:cl #:parachute)
  (:shadow #:run)
  (:nicknames #:org.shirakumo.array-utils.test))

(in-package #:org.shirakumo.array-utils.test)

(defun cp (thing)
  (etypecase thing
    (array (make-array (array-dimensions thing)
                       :initial-contents thing
                       :adjustable (adjustable-array-p thing)
                       :fill-pointer (fill-pointer thing)))))

(defvar *array* (make-array 4 :initial-contents #(0 1 2 3) :adjustable T :fill-pointer T))

(defun as (&rest params &key &allow-other-keys)
  (apply #'array-utils:array-shift (cp *array*) params))

(defun eas (&rest params)
  (apply #'array-utils:ensure-array-size (cp *array*) params))

(defmacro same (form exp)
  `(is equalp ,exp ,form))

(define-test array-utils)

(define-test ensure-array-size
  :parent array-utils
  (same (eas 4) #(0 1 2 3))
  (same (eas 3) #(0 1 2))
  (same (eas 5) #(0 1 2 3 0)))

(define-test array-shift
  :parent array-utils
  ;;; Check errors
  (fail (array-utils:array-shift NIL))
  (fail (as :n 0.0))
  (fail (as :from -1))
  (fail (as :to -1))
  (fail (as :from 2 :to 1))
  (fail (as :n 1 :contents #()))
  ;;; Positive shifting
  (same (as :n 0)
        *array*)
  (same (as :n 1 :adjust NIL)
        #(0 0 1 2))
  (same (as :n 1 :adjust T)
        #(0 0 1 2 3))
  (same (as :n 1 :adjust NIL :from 1)
        #(0 1 1 2))
  (same (as :n 1 :adjust T :from 1)
        #(0 1 1 2 3))
  (same (as :n 1 :adjust NIL :to 2)
        #(0 0 1 3))
  (same (as :n 1 :adjust T :to 2)
        #(0 0 1 3))
  (same (as :n 1 :adjust NIL :from 1 :to 3)
        #(0 1 1 2))
  (same (as :n 1 :adjust T :from 1 :to 3)
        #(0 1 1 2))
  (same (as :n 3 :adjust NIL :to 1)
        #(0 1 2 0))
  (same (as :n 5 :adjust NIL)
        #(0 1 2 3))
  (same (as :n 4 :adjust T :to 1)
        #(0 1 2 3 0))
  (same (as :n 4 :adjust T :from 1 :to 2)
        #(0 1 2 3 0 1))
  ;; With filling
  (same (as :n 1 :adjust NIL :fill NIL)
        #(NIL 0 1 2))
  (same (as :n 5 :adjust NIL :fill NIL)
        #(NIL NIL NIL NIL))
  (same (as :n 1 :adjust NIL :from 1 :fill NIL)
        #(0 NIL 1 2))
  ;; With contents
  (same (as :n 2 :adjust NIL :contents #(5 6))
        #(5 6 0 1))
  (same (as :n 2 :adjust NIL :contents #(5 6 7))
        #(5 6 0 1))
  ;; With contents and filling
  (same (as :n 2 :adjust NIL :fill NIL :contents #(5))
        #(5 NIL 0 1))
  (same (as :n 2 :adjust NIL :fill NIL :contents #(5 6))
        #(5 6 0 1))
  ;;; Negative shifting
  (same (as :n -1 :adjust NIL)
        #(1 2 3 3))
  (same (as :n -1 :adjust T)
        #(1 2 3))
  (same (as :n -1 :adjust NIL :from 2)
        #(0 2 3 3))
  (same (as :n -1 :adjust T :from 2)
        #(0 2 3))
  (same (as :n -1 :adjust NIL :to 3)
        #(1 2 2 3))
  (same (as :n -1 :adjust T :to 3)
        #(1 2 2 3))
  (same (as :n -1 :adjust NIL :from 2 :to 3)
        #(0 2 2 3))
  (same (as :n -1 :adjust T :from 2 :to 3)
        #(0 2 2 3))
  (same (as :n -3 :adjust NIL :from 3 :to 4)
        #(3 1 2 3))
  (same (as :n -5 :adjust NIL)
        #(0 1 2 3))
  (same (as :n -3 :adjust T :from 3 :to 4)
        #(3 1 2))
  ;; With filling
  (same (as :n -1 :adjust NIL :fill NIL)
        #(1 2 3 NIL))
  (same (as :n -5 :adjust NIL :fill NIL)
        #(NIL NIL NIL NIL))
  (same (as :n -1 :adjust NIL :to 3 :fill NIL)
        #(1 2 NIL 3))
  ;; With contents
  (same (as :n -2 :adjust NIL :contents #(5 6))
        #(2 3 5 6))
  (same (as :n -2 :adjust NIL :contents #(5 6 7))
        #(2 3 5 6))
  ;; With contents and filling
  (same (as :n -2 :adjust NIL :fill NIL :contents #(5))
        #(2 3 5 NIL))
  (same (as :n -2 :adjust NIL :fill NIL :contents #(5 6))
        #(2 3 5 6)))

(define-test vector-push-extend-front
  :parent array-utils
  :depends-on (vector-push-extend-position)
  (let ((arr (cp *array*)))
    (same (array-utils:vector-push-extend-front 5 arr)
          5)
    (same arr
          #(5 0 1 2 3))))

(define-test vector-push-extend-position
  :parent array-utils
  :depends-on (array-shift)
  (let ((arr (cp *array*)))
    (same (array-utils:vector-push-extend-position 5 arr 1)
          5)
    (same arr
          #(0 5 1 2 3))))

(define-test vector-pop-front
  :parent array-utils
  :depends-on (vector-pop-position)
  (let ((arr (cp *array*)))
    (same (array-utils:vector-pop-front arr)
          0)
    (same arr
          #(1 2 3)))
  (fail (array-utils:vector-pop-front #())))

(define-test vector-pop-front*
  :parent array-utils
  :depends-on (vector-pop-position*)
  (let ((arr (cp *array*)))
    (same (array-utils:vector-pop-front* arr)
          0)
    (same arr
          #(3 1 2)))
  (fail (array-utils:vector-pop-front #())))

(define-test vector-pop-position
  :parent array-utils
  :depends-on (array-shift)
  (let ((arr (cp *array*)))
    (same (array-utils:vector-pop-position arr 1)
          1)
    (same arr
          #(0 2 3)))
  (let ((arr (cp *array*)))
    (same (array-utils:vector-pop-position arr 3) 3)
    (same arr
          #(0 1 2)))
  (fail (array-utils:vector-pop-position (cp *array*) -1))
  (fail (array-utils:vector-pop-position (cp *array*) 5)))

(define-test vector-pop-position*
  :parent array-utils
  (let ((arr (cp *array*)))
    (same (array-utils:vector-pop-position* arr 1)
          1)
    (same arr
          #(0 3 2)))
  (let ((arr (cp *array*)))
    (same (array-utils:vector-pop-position arr 3) 3)
    (same arr
          #(0 1 2)))
  (fail (array-utils:vector-pop-position* (cp *array*) -1))
  (fail (array-utils:vector-pop-position* (cp *array*) 5)))

(define-test vector-append
  :parent array-utils
  :depends-on (array-shift)
  (same (array-utils:vector-append (cp *array*) #(4 5 6))
        #(0 1 2 3 4 5 6))
  (same (array-utils:vector-append (cp *array*) '(4 5 6))
        #(0 1 2 3 4 5 6))
  (same (array-utils:vector-append #() #())
        #()))
