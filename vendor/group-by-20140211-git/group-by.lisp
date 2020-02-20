(defpackage :group-by
  (:use :cl :cl-user :iterate)
  (:export
   :group-by
   :categorize-item
   :grouped-list
   :make-grouped-list
   :add-item-to-grouping
   :key-value
   :child-groupings
   :items-in-group
   :parent-grouping
   :keys :tests
   :make-child-grouped-list
   :group-by-repeated
   :grouped-list-speed-tester))

(in-package :group-by)

(defun group-by (list &key (key #'car) (value #'cdr) (key-fn #'identity) (test #'equal))
  "groups the list into an alist using the key function and value function to group by key,
with a list of all values for that key.

key is used to determine the key in the a-list
value is used to determin the value in the a-list
key-fn is passed as the :key to assoc
test is passed as the :test to assoc

eg: (group-by '((a 1 2) (a 3 4) (b 5 6)))
=> ((A (1 2) (3 4)) (B (5 6)))"
  ;; we keep 2 alist -  ( key . value-list-head ) & ( key . value-list-tail )
  ;; so we can collect at the end (which saves us infinitesimal time and space)
  (iter (for i in list)
    (for k = (funcall key i))
    (for v = (cons (funcall value i) nil))
    (for cell = (assoc k tails :test test :key key-fn))
    (cond
      (cell (setf (cddr cell) v
                  (cdr cell) v))
      (t               ;; dont reuse this cons cell, we want two distinct ones
       (collect (cons k v) into results)
       (collect (cons k v) into tails)))
    (finally (return results))))

(defgeneric categorize-item (item root &key &allow-other-keys )
  (:documentation "Insert a new item into a grouped list "))

(defmethod categorize-item (item (root list) &key keys tests &allow-other-keys)
  "Categorize a new item into an alist as produced by group-by-repeated
     This will create new category nodes if necessary"
  (if (null keys)
      (push item root)
      (let ((key (funcall (first keys) item)))
        (let ((data (assoc key root :test (or (first tests) #'equal))))
          (if data
              ;; Add the rest of the categorization to the
              ;; data of this item
              (setf (cdr data) (categorize-item
                                item (cdr data)
                                :keys (rest keys)
                                :tests (rest tests)))
              ;; we have no data for this node, build a new subtree
              (push (cons key (categorize-item
                               item nil
                               :keys (rest keys)
                               :tests (rest tests)))
                    root)))))
  root)

(defun group-by-repeated (list &key keys tests)
  "Returns an alist tree that represents the items in the list as categorized
   by keys (compared with tests)
    ex: ((a 3 sam) (c 4 bob) (a 3 ted))


   keys: a list of key functions that describe the categorizations in order
   tests: how we are testing whether or not two keys are equal, defaults to #'equal
  "
  (let (root)
    (iter (for item in list)
          (setf root (categorize-item item root :keys keys :tests tests)))
    root))

(defclass grouped-list ()
  ((orig-list :accessor orig-list :initarg :orig-list :initform nil)
   (grouping-implementation
    :accessor grouping-implementation :initarg :grouping-implementation :initform :list
    :documentation
    "What data structure should be used to perform the grouping :list, :hash-table")
   (keys :accessor keys :initarg :keys :initform nil
         :documentation "A list of key functions we will use to group the list")
   (tests :accessor tests :initarg :tests :initform nil
          :documentation "A list of test functions we will use to test key equality
      tree: defaults to #'equal
      hash-table: this be a single hash-equality symbol (defaults to 'equal)")
   (%child-groupings :accessor %child-groupings :initarg :%child-groupings :initform nil)
   (%items :accessor %items :initarg :%items :initform nil)
   (parent-grouping :accessor parent-grouping :initarg :parent :initform nil
    :documentation "If this is a subgrouping of another grouped-list, what is the parent grouping we are apart of (mostly for testing)")
   (key-value :accessor key-value :initarg :key-value :initform nil
    :documentation "If this is a subgrouping of another grouped-list, what is the key this grouped-list represents in the parent grouping (mostly for testing)"))
  (:documentation "This class represents a list that we have grouped by multiple key values
     ala one of the group-by-repeatedly functions "))

(defgeneric child-groupings (grouped-list)
  (:method ((gl grouped-list))
    (case (grouping-implementation gl)
      (:hash-table (iter (for (k v) in-hashtable (%child-groupings gl))
                     (collect v)))
      (T (%child-groupings gl)))))

(defun make-grouped-list (inp &key tests keys (grouping-implementation :alist))
  "Given a list of input, produce a grouped-list CLOS object that contains
the original list, configuration about the groupings and the result tree
of grouped-list objects

''keys'': a list of keys to group by<br />
''tests'': a list of tests to compare the keys with<br />

''grouping-implmentation'': What data structure should be used to perform the grouping<br />
  '':alist, :tree , :hash-table''<br />
  The implementation doesnt change the output, but it does change
  the performance characteristics of the grouped-object (see:
  grouped-list-speed-tester for help deciding which to use)
  "
  (make-instance 'grouped-list
                 :tests tests
                 :keys keys
                 :grouping-implementation grouping-implementation
                 :list inp))

(defmethod initialize-instance :after ((o grouped-list) &key list &allow-other-keys)
  (unless (listp (keys o)) (setf (keys o) (list (keys o))))
  (unless (listp (tests o)) (setf (tests o) (list (tests o))))
  (when (eql :hash-table (grouping-implementation o))
    (setf (%child-groupings o)
          (make-hash-table :test (or (first (tests o)) 'equal))))

  (when list ;; only do this if we are not a child-grouped-list
    (setf (orig-list o) list)
    (iter (for x in list)
      (add-item-to-grouping x o))))

(defun find-single-sub-category (gl key-value &key test)
  (case (grouping-implementation gl)
      (:hash-table (gethash key-value (%child-groupings gl)))
      (t (find key-value (%child-groupings gl) :key #'key-value :test test))))

(defmethod categorize-item (item (root grouped-list) &key &allow-other-keys)
  (iter
    (with node = root)
    (with tests = (tests root))
    (with keys = (keys root))
    (for keyfn in keys)
    (for testfn = (or (first tests) #'equal))
    (setf tests (rest tests))
    (for key = (funcall keyfn item))
    (setf node
          (or (find-single-sub-category node key :test testfn)
              (make-child-grouped-list node key item)))
    (finally
     (push item (%items node))))
  root)

(defgeneric add-item-to-grouping (item grouped-list)
  (:method (item (gl grouped-list))
    "puts a new item in the grouping of the grouped list (but not in the original list)"
    (categorize-item item gl)))

(defgeneric %grouping-items (grouped-list)
  (:method ((gl grouped-list))
    "Returns the items in a given group"
    (append (%items gl)
            (iter
              (for cgl in (child-groupings gl))
              (nconcing (%grouping-items cgl))))))

(defmethod make-child-grouped-list ((gl grouped-list) key-value grouped-list)
  (let ((c (make-instance
            'grouped-list
            :orig-list (orig-list gl)
            :keys (rest (keys gl))
            :tests (rest (tests gl))
            :grouping-implementation (grouping-implementation gl)
            :parent-grouping gl
            :key-value key-value)))
    (case (grouping-implementation gl)
      (:hash-table
       (setf (gethash key-value (%child-groupings gl)) c))
      (t (push c (%child-groupings gl))))
    c))

(defgeneric items-in-group (grouped-list &rest keys)
  (:documentation
   " a list of key values that will produce a list of all the items in a given group")
  (:method ((gl grouped-list) &rest key-values)
    (let ((subgroup gl)
          (tests (tests gl)))
      (iter
        (for key in key-values)
        (for test = (or (first tests) #'equal))
        (setf tests (rest tests))
        (setf subgroup (find-single-sub-category subgroup key :test test)))

      ;; Get all the items for that subgrouping (for alists this is a list we just produced)
      ;; and that list will simply pass through
      (%grouping-items subgroup))))

(defun grouped-list-speed-tester (&key list keys tests hash-tests (iterations 10) actions)
  "A function to help assess which implementation will work best in your given scenario
   actions : (lambda (gl) ...) -to help test whatever grouped list
             operations you will need to do repeatedly

  "
  (format *trace-output* "Grouping Implentation Speed Tests" )
  (format *trace-output* "~%~%HASH-TABLE Implementation~%" )
  (time
   (iter (for i from 1 to iterations)
         (let ((gl (make-instance
                    'grouped-list
                    :list list :keys keys :tests hash-tests
                    :grouping-implementation :hash-table)))
           (when actions (funcall actions gl)))))
  (format *trace-output* "~%~%LIST Implementation~%" )
  (time
   (iter (for i from 1 to iterations)
         (let ((gl (make-instance 'grouped-list :list list :keys keys :tests tests
                        :grouping-implementation :list)))
           (when actions (funcall actions gl)))
         )))
