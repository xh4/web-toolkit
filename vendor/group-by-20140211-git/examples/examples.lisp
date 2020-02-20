(in-package :group-by)

(defparameter +example-timeclock-data+
  `(("russ" 1 :proj-a)
    ("russ" 2 :proj-a)
    ("bob" 1  :proj-a)
    ("russ" 2 :proj-b)
    ("bob" 1 :proj-b)
    ("bob" 1 :proj-b)
    ("russ" 2  :proj-b)
    ("bob" 1 :proj-c)
    ("russ" 4 :proj-c)))

(group-by +example-timeclock-data+)
;; results in
'(("russ"
   (1 :proj-a) (2 :proj-a) (2 "time on proj b")
   (2 "time on proj b") (4 :proj-c))
  ("bob"
   (1 :proj-a) (1 "time on proj b") (1 "time on proj b")
   (1 :proj-c)))

(defparameter +example-multiple-grouped-timeclock-data+
  (group-by-repeated
   +example-timeclock-data+
   :keys (list #'first #'third)
   :tests (list #'string-equal #'eql)))
;; results in
'(("bob"
   (:proj-c ("bob" 1 :proj-c))
   (:proj-b ("bob" 1 :proj-b) ("bob" 1 :proj-b))
   (:proj-a ("bob" 1 :proj-a)))
  ("russ"
   (:proj-c ("russ" 4 :proj-c))
   (:proj-b ("russ" 2 :proj-b) ("russ" 2 :proj-b))
   (:proj-a ("russ" 2 :proj-a) ("russ" 1 :proj-a))))

(defparameter +example-grouped-list-timeclock-data-alist+
  (make-grouped-list
   +example-timeclock-data+
   :keys (list #'first #'third)
   :tests (list #'string-equal #'eql)))

(defclass example-timeclock-record ()
  ((name :accessor name :initarg :name :initform nil)
   (hours :accessor hours :initarg :hours :initform 0)
   (proj :accessor proj :initarg :proj :initform nil)))

(defun example-tcr (name hours proj)
  (make-instance 'example-timeclock-record :name name :hours hours :proj proj))

(defparameter +example-timeclock-objs+
  (iter top (for i from 0 to 10)
        (iter (for rec in +example-timeclock-data+)
              (in top (collect (apply #'example-tcr rec))))))

(defun timeclock-report-rec-print (gl &optional (spaces ""))
  (let ((further-groups (child-groupings gl)))
    (if further-groups
        (iter
          (with hours = 0)
          (for group in further-groups)
          (format T "~?~A~%" spaces () (key-value group) )
          (incf hours (timeclock-report-rec-print
                       group (concatenate 'string spaces "~2,1@T")))
          (finally
           (format T "~?Total: ~D~%" spaces () hours )
           (return hours)))
        (iter (for kid in (items-in-group gl))
              (sum (hours kid) into hours)
              (finally
               (format T "~?Total: ~D~%" spaces () hours )
               (return hours))))))

(defun print-timeclock-report ()
  (let ((by-person-project
         (make-grouped-list
          +example-timeclock-objs+
          :keys (list #'name #'proj)
          :tests (list #'equalp #'eql)
          :grouping-implementation :hash-table))
        (by-project-person
         (make-grouped-list
          +example-timeclock-objs+
          :keys (list #'proj #'name)
          :tests (list #'eql #'equalp)
          :grouping-implementation :list)))

    (format T "Hours BY Project > Person~%-----------~%")
    (timeclock-report-rec-print by-project-person)
    (format T "~%~%Hours BY Person > Project~%-----------~%")
    (timeclock-report-rec-print by-person-project)
    ))

#|
Hours BY Project > Person
-----------
PROJ-C
  russ
    Total: 44
  bob
    Total: 11
  Total: 55
PROJ-B
  bob
    Total: 22
  russ
    Total: 44
  Total: 66
PROJ-A
  bob
    Total: 11
  russ
    Total: 33
  Total: 44
Total: 165


Hours BY Person > Project
-----------
russ
  PROJ-A
    Total: 33
  PROJ-B
    Total: 44
  PROJ-C
    Total: 44
  Total: 121
bob
  PROJ-A
    Total: 11
  PROJ-B
    Total: 22
  PROJ-C
    Total: 11
  Total: 44
Total: 165
|#


(defparameter +example-speedtest-timeclock-objs+
  (iter top (for i from 0 to 100)
        (iter (for rec in +example-timeclock-data+)
              (in top (collect (apply #'example-tcr rec))))))

(defun speed-test-example ()
  (format *trace-output* "~%build-gl-speed-test~%")
  (grouped-list-speed-tester
   :iterations 100
   :list +example-speedtest-timeclock-objs+
   :keys (list #'name #'proj)
   :tests (list #'string-equal #'eql))

  (format *trace-output* "~%~%build-gl-speed-test with-item-access
This shows how implementations differ based on workload~%")
  (grouped-list-speed-tester
   :iterations 10
   :list +example-speedtest-timeclock-objs+
   :keys (list #'name #'proj)
   :tests (list #'string-equal #'eql)
   :actions (lambda (gl)
              (iter (for c from 0 to 1000 )
                    (iter
                      (for i in '("russ" "bob"))
                      (items-in-group gl i)
                      (iter (for j in `(:proj-a :proj-b :proj-c))
                            (items-in-group gl i j)))))))

#|
build-gl-speed-test
Grouping Implentation Speed Tests

HASH-TABLE Implementation
Evaluation took:
  0.181 seconds of real time
  0.160000 seconds of total run time (0.120000 user, 0.040000 system)
  [ Run times consist of 0.080 seconds GC time, and 0.080 seconds non-GC time. ]
  88.40% CPU
  92 lambdas converted
  451,161,165 processor cycles
  4,939,600 bytes consed
  


LIST Implementation
Evaluation took:
  0.107 seconds of real time
  0.100000 seconds of total run time (0.080000 user, 0.020000 system)
  93.46% CPU
  46 lambdas converted
  265,815,870 processor cycles
  2,749,600 bytes consed
  


build-gl-speed-test with-item-access
This shows how implementations differ based on workload
Grouping Implentation Speed Tests

HASH-TABLE Implementation
Evaluation took:
  0.873 seconds of real time
  0.860000 seconds of total run time (0.760000 user, 0.100000 system)
  [ Run times consist of 0.330 seconds GC time, and 0.530 seconds non-GC time. ]
  98.51% CPU
  2,175,013,267 processor cycles
  294,658,896 bytes consed
  


LIST Implementation
Evaluation took:
  1.031 seconds of real time
  0.990000 seconds of total run time (0.900000 user, 0.090000 system)
  [ Run times consist of 0.460 seconds GC time, and 0.530 seconds non-GC time. ]
  96.02% CPU
  2,572,489,710 processor cycles
  293,604,720 bytes consed
|#

(defparameter +example-names+ #("russ" "alice" "bob" "charlie"))
(defparameter +example-projects+ #(:proj-a :proj-b :proj-c :proj-d))

(defun incremental-grouping-examples ()
  "An example of building a grouped list up from individual items
   rather than starting with a full list then grouping it"
  (iter (for type in `(:hash-table :tree :alist))
        (iter
          (with gl = (make-grouped-list
                      nil
                      :keys (list #'name #'proj)
                      :tests (list #'string-equal #'eql)
                      :grouping-implementation type))
          (for i from 0 to 1000)
          (for tcr =
               (example-tcr
                (alexandria:random-elt +example-names+)
                (random 10)
                (alexandria:random-elt +example-projects+)))
          (add-item-to-grouping tcr gl)
          (finally (timeclock-report-rec-print gl)
                   (format T "----------------------~%")))))