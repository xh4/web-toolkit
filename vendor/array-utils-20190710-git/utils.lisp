#|
This file is a part of Array-Utils
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:array-utils
  (:nicknames #:org.shirakumo.array-utils)
  (:use #:cl)
  (:export
   #:ensure-array-size
   #:array-shift
   #:vector-push-extend-front
   #:vector-push-extend-position
   #:vector-pop-front
   #:vector-pop-front*
   #:vector-pop-position
   #:vector-pop-position*
   #:vector-append))
(in-package #:org.shirakumo.array-utils)

(defun ensure-array-size (array new-space)
  (when (array-has-fill-pointer-p array)
    (unless (array-in-bounds-p array new-space)
      (adjust-array array new-space))
    (incf (fill-pointer array)
          (- new-space (fill-pointer array))))
  array)

(deftype positive-fixnum ()
  `(and fixnum (integer 0)))

(defun array-shift (array &key (n 1) (from 0) to (adjust T) (fill NIL f-p) (contents NIL c-p))
  "Shifts a subset of array elements in either direction for a specified amount.
Optionally also extends the array and fills empty space with a given element.

N        --- The amount to be moved. If positive, things are shifted to the right. If
             negative, things are shifted to the left.
FROM     --- The left point of the region to move, inclusive.
TO       --- The right point of the region to move, exclusive.
ADJUST   --- Whether to adjust the fill pointer and the array bounds. The array is only
             adjusted if N is positive and the range of TO+N would exceed the ARRAY length,
             or if N is negative and TO equals the length of the ARRAY
FILL     --- If provided, empty spaces created by the move will be filled with this element.
CONTENTS --- If provided, uses the contents to fill the new space. If |N| is greater than the
             length of this sequence, FILL is used to fill the rest of the space if it is
             provided. If not, an error is signalled. No matter whether N is negative or
             positive, the content is filled in from left to right in the order it is given."
  (declare (optimize speed))
  (check-type n fixnum)
  (check-type from positive-fixnum)
  (check-type to (or null positive-fixnum))
  (check-type array vector)
  (let* ((length (length array))
         (to (or to length))
         (amount (abs n))
         (direction (cond ((< n 0) :left)
                          ((< 0 n) :right)
                          (T :nowhere)))
         (content-length (length contents)))
    (declare (type positive-fixnum from to amount content-length)
             (type vector array))
    (assert (<= from to) () "FROM must be smaller or equal to TO.")
    (assert (or f-p (not c-p) (<= amount content-length)) (fill)
            "FILL is not provided and CONTENTS is smaller than the amount to shift.")
    (case direction
      (:right
       (when (and adjust (<= length (+ to amount)))
         (ensure-array-size array (+ to amount))
         (setf length (+ to amount)))
       (loop for cursor of-type positive-fixnum
             from (1- (min length (+ to amount)))
             downto (+ from amount)
             do (setf (aref array cursor)
                      (aref array (the positive-fixnum (- cursor amount)))))
       (when c-p
         (macrolet ((iterate (iteration)
                      `(loop for cursor of-type positive-fixnum
                             from from below (min (+ from amount) to)
                             for item ,iteration contents
                             do (setf (aref array cursor) item))))
           (etypecase contents
             (list (iterate in))
             (vector (iterate across)))))
       (when f-p
         (loop repeat (- amount content-length)
               for cursor of-type positive-fixnum
               from (+ from content-length) below (min length (+ to amount))
               do (setf (aref array cursor) fill))))
      (:left
       (when (< 0 (- to amount))
         (loop for cursor of-type positive-fixnum
               from (max 0 (- from amount))
               upto (1- (- to amount))
               do (setf (aref array cursor)
                        (aref array (the positive-fixnum (+ cursor amount))))))
       (when (and adjust (= to length))
         (ensure-array-size array (max from (- to amount)))
         (setf length (max from (- to amount))))
       (when c-p
         (macrolet ((iterate (iteration)
                      `(loop for cursor of-type positive-fixnum
                             from (max (- to amount) from) below to
                             for item ,iteration contents
                             do (setf (aref array cursor) item))))
           (etypecase contents
             (list (iterate in))
             (vector (iterate across)))))
       (when f-p
         (loop for cursor of-type positive-fixnum
               from (+ content-length (max (- to amount) from)) below to
               do (setf (aref array cursor) fill))))))
  array)

(defun vector-push-extend-position (element vector position)
  "Pushes the element into the specified position and shifts everything
to the right to make space. This is potentially very costly as all
elements after the given position need to be shifted as per ARRAY-SHIFT."
  (array-shift vector :n 1 :from position)
  (setf (aref vector position) element)
  (fill-pointer vector))

(defun vector-push-extend-front (element vector)
  "Pushes the element onto the front of the vector and extends if necessary.
This operation is very costly and takes O(n) time as each element needs to
be shifted as per ARRAY-SHIFT.

See VECTOR-PUSH-EXTEND-POSITION"
  (vector-push-extend-position element vector 0))

(defun vector-pop-position (vector position)
  "Pops the element at the given position of the vector and returns it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT.

See VECTOR-POP-POSITION*"
  (if (= (1- (length vector)) position)
      (vector-pop vector)
      (prog1 (aref vector position)
        (array-shift vector :n -1 :from (1+ position)))))

(defun vector-pop-position* (vector position)
  "Pops the element at the given position of the vector and returns it.
This is faster than VECTOR-POP-POSITION, but does not preserve the order of elements
in the vector.

See VECTOR-POP-POSITION"
  (decf (fill-pointer vector))
  (shiftf (aref vector position) (aref vector (length vector))))

(defun vector-pop-front (vector)
  "Pops the first element off the vector and returns it.
This operation is very costly and takes O(n) time as each element needs to
be shifted as per ARRAY-SHIFT.

See VECTOR-POP-FRONT*
See VECTOR-POP-POSITION"
  (vector-pop-position vector 0))

(defun vector-pop-front* (vector)
  "Pops the first element off the vector and returns it.
This is faster than VECTOR-POP-FRONT, but does not preserve the order of elements
in the vector.

See VECTOR-POP-FRONT
See VECTOR-POP-POSITION"
  (vector-pop-position* vector 0))

(defun vector-append (vector sequence &optional position)
  "Appends all elements of the sequence at position of the vector and returns it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT."
  (let ((position (or position (length vector))))
    (array-shift vector :n (length sequence) :from position)
    (macrolet ((iterate (iteration)
                 `(loop for i from position
                        for item ,iteration sequence
                        do (setf (aref vector i) item))))
      (etypecase sequence
        (list (iterate in))
        (vector (iterate across))))
    vector))
