(defpackage :group-by-test
  (:use :cl :cl-user :group-by :lisp-unit2 :iter))

(in-package :group-by-test)

(defvar +lower-case-ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyz"
  "All the lower case letters in 7 bit ASCII.")

(defun random-string (&optional (length 32) (alphabet +lower-case-ascii-alphabet+))
  "Returns a random alphabetic string.

The returned string will contain LENGTH characters chosen from
the vector ALPHABET.
"
  (iter (with id = (make-string length))
        (with alphabet-length = (length alphabet))
        (for i below length)
        (setf (cl:aref id i)
              (cl:aref alphabet (random alphabet-length)))
        (finally (return id))))

(defun info (message &rest arguments)
  (format *standard-output* message arguments))

(defun test-data (&optional (num-rows 25) (depth 5))
  " This function returns (:list data :keys keys :tests tests)
    for application to make-instance in  make-test-data-instance

    the data returned is a list of arrays with keys representing
    each successive datum in the array.  These datums are alternating
    random strings and numbers with bounds such there there should
    always be some overlap
  "
  (list
   :list
   (let ((alphabet (if (< num-rows (length +lower-case-ascii-alphabet+))
                       (subseq +lower-case-ascii-alphabet+
                               0 (floor (+ 1 (/ num-rows 3))))
                       +lower-case-ascii-alphabet+)))
     (iter (for i from 1 to num-rows)
           (collect (apply #'vector
                           (iter (for i from 1 to depth)
                                 (collect
                                     (case (mod i 2)
                                       (0 (random-string
                                           (truncate (+ 1 (/ num-rows (length alphabet) 3)))
                                           alphabet))
                                       (1 (random (max (/ num-rows 10) 3))))))))))
   :keys (iter (for i from 0 below depth)
               (collect (alexandria:rcurry #'elt i)))
   :tests (iter (for i from 1 to depth)
                (collect
                    (case (mod i 2)
                      (0 #'equalp)
                      (1 #'eql))))))

(defmethod print-object ((o grouped-list) s)
  ;; This is way slow so dont have this in live code and you might wish to undefine it
  ;; for more accurate speed tests
  (print-unreadable-object (o s :type t :identity t)
    (format s "p-key:~a " (key-value o))
    (format s "num-kids:~a " (length (child-groupings o)))
    (format s "num-data:~a " (length (items-in-group o)))))

(defun make-test-data-instance (test-data &rest other-keywords)
  (let* ((args (append (list 'grouped-list)
                       test-data
                       other-keywords))
         (o (apply #'make-instance args)))
    o
    ))

(defparameter +test-timeclock-data+
  `(("Russ" 1 "time on proj A")
    ("Russ" 2 "time on proj A")
    ("Bob" 1  "time on proj A")
    ("Russ" 2 "time on proj B")
    ("Bob" 1 "time on proj B")
    ("Bob" 1 "time on proj B")
    ("Russ" 2  "time on proj B")
    ("Bob" 1 "time on proj C")
    ("Russ" 4 "time on proj C")))

(define-test basic-group-by ()
  (let ((res (group-by +test-timeclock-data+)))
    (assert-equal 2 (length res) "Grouped by the two employees")
    (assert-equal
     "Russ" (car (first res))
     "items should have same order, Russ should be first key")
    (assert-equal
     5 (length (cdr (assoc "Russ" res :test #'string-equal)))
     "russ had 5 records")
    (assert-equal "Bob" (car (second res)))
    (assert-equal
     4 (length (cdr (assoc "Bob" res :test #'string-equal)))
     "Bob had 4 records")
    ))

(define-test basic-group-by-2 ()
  (let* ((res (group-by (second (test-data 25 3))
                        :key #'(lambda (s) (elt s 1))
                        :value #'identity
                        :test #'string-equal)))
    (iter (for (k . data) in res)
          (iter (for s in data)
                (assert-equal k (elt s 1)))
          (summing (length data) into row-cnt)
          (finally
           (assert-equal 25 row-cnt)))
    ))

(define-test basic-grouped-list ()
  (let* ((res (make-grouped-list
               (second (test-data 25 3))
               :keys (list #'(lambda (s) (elt s 1))))))
    (iter
      (for gl in (child-groupings res))
      (for kv = (key-value gl))
      (iter (for item in (items-in-group gl))
        (assert-equal kv (elt item 1))))
    (assert-equal 25 (length (items-in-group res)))
    ))

(define-test basic-grouped-list2 ()
  (assert-true
   (apply #'make-instance 'grouped-list (test-data 25 3))))

(define-test run-accuracy-tests ()
  (let ((num-rows 1000) (depth 5))
    (labels ((assertions (g1 g2)
               ;; all grouped lists contain the same number of children
               (assert-equal
                   (length (items-in-group g1))
                   (length (items-in-group g2)))

               ;; all grouped lists contain the same data
               (assert-true (null (set-difference
                                    (items-in-group g1)
                                    (items-in-group g2)
                                    :test #'equalp )))

               ;; assert that all children should actually be in this group
               (when (parent-grouping g1)
                 (let ((pk (key-value g1)))
                   (when pk
                     (let* ((test (first (tests (parent-grouping g1))))
                            (key (first (keys (parent-grouping g1)))))
                       (iter (for item in (items-in-group g1))
                             (assert-true (funcall test (funcall key item) pk))))))))

             ;; A function to run the tests on each sub grouping tree
             (recursert (g1 g2)
               ;;(break "recursert:~%~a~%~a~%~a" g1 g2 g3)
               (assertions g1 g2)
               (let* ((k1 (child-groupings g1))
                      (k2 (child-groupings g2))
                      (pred (when k1
                              (if (numberp (key-value (first k1)))
                                  #'<
                                  #'string<))))
                 (when k1
                   (setf k1 (sort k1 pred :key #'key-value))
                   (setf k2 (sort k2 pred :key #'key-value)))
                 ;(break "~A" (list k1 k2 k3))
                 (iter (for kg1 in k1)
                       (for kg2 in k2)
                       (recursert kg1 kg2)))))
      (let* ((data (test-data num-rows depth))
             (lgl (make-test-data-instance data :grouping-implementation :list))
             (hgl (make-test-data-instance data :grouping-implementation :hash-table)))
        (recursert lgl hgl)))))

(defun %run-creation-speed-tests (&key (num-rows 1000) (depth 5) (times 10))
  (macrolet ((time-to-log (&body body)
               `(info
                 (with-output-to-string (*trace-output*)
                   (time (progn ,@body))))))
    (let ((test-data (iter (for i from 1 to times)
                           (collect (test-data num-rows depth)))))
      (info "Grouping Implentation Speed Tests" )
      (info "~%HASH-TABLE Implementation~%" )
      (time-to-log
       (iter (for data in test-data)
             (make-test-data-instance data :grouping-implementation :hash-table)))

      (info "~%LIST Implementation~%" )
      (time-to-log
       (iter (for data in test-data)
             (make-test-data-instance data :grouping-implementation :list)))

      )))

(define-test run-creation-speed-tests (:tags '(speed))
  (%run-creation-speed-tests))

