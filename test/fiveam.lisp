(in-package :test)

(defvar *it* nil)
(defvar *is* nil)

(defmacro => (n form)
  `(let ((*is* ',(append `(=> ,n) (list form))))
     ,form))

(defmacro it (&body body)
  (setf body
        (let ((n 1))
          (map-tree
           (lambda (form)
             (if (and (listp form)
                      (not (= 0 (length form)))
                      (member (first form) '(fiveam:is fiveam:is-true
                                             fiveam:is-every fiveam:is-false
                                             fiveam:signals fiveam:finishes
                                             fiveam:pass fiveam:fail)))
                 (throw :skip (prog1
                                  (append `(=> ,n) (list form))
                                (incf n)))
                 form))
           body
           :tag :skip)))
  `(let ((*it* ',body))
     ,@body))

(in-package :fiveam)
(defun process-failure (test-expr &optional reason-format &rest format-args)
  (setf test:*it*
        (test::map-tree
         (lambda (form)
           (if (and (listp form)
                    (not (emptyp form))
                    (equal (first form) 'test:=>))
               (throw :skip
                 (if (equal (second test:*is*) (second form))
                     (cons 'test:=> (cddr form))
                     (cddr form)))
               form))
         test:*it*
         :tag :skip))
  (let ((reason (with-output-to-string (stream)
                  (format stream "~%IN")
                  (loop for form in test:*it*
                     do (format stream "~%")
                       (pprint form stream))
                  (when reason-format
                    (apply 'format stream reason-format format-args)))))
    (with-simple-restart (ignore-failure "Continue the test run.")
      (error 'check-failure :test-expr test-expr
             :reason reason))
    (add-result 'test-failure :test-expr test-expr
                :reason reason)))

(defmethod run-test-lambda ((test test-case))
  (with-run-state (result-list)
    (bind-run-state ((current-test test))
      (labels ((abort-test (e &optional (reason (format nil "Unexpected Error: ~S~%~A." e e)))
                 (setf test:*it*
                       (test::map-tree
                        (lambda (form)
                          (if (and (listp form)
                                   (not (emptyp form))
                                   (equal (first form) 'test:=>))
                              (throw :skip
                                (if (equal (second test:*is*) (second form))
                                    (cons 'test:=> (cddr form))
                                    (cddr form)))
                              form))
                        test:*it*
                        :tag :skip))
                 (let ((reason (with-output-to-string (stream)
                                 (format stream "~%IN")
                                 (loop for form in test:*it*
                                    do (format stream "~%")
                                      (pprint form stream))
                                 (format stream "~%~%~A" reason))))
                   (add-result 'unexpected-test-failure
                               :test-expr nil
                               :test-case test
                               :reason reason
                               :condition e)))
               (run-it ()
                 (let ((result-list '()))
                   (declare (special result-list))
                   (handler-bind ((check-failure (lambda (e)
                                                   (declare (ignore e))
                                                   (cond
                                                     ((eql *on-failure* :debug)
                                                      nil)
                                                     (t
                                                      (when (eql *on-failure* :backtrace)
                                                        (trivial-backtrace:print-backtrace-to-stream
                                                         *test-dribble*))
                                                      (invoke-restart
                                                       (find-restart 'ignore-failure))))))
                                  (error (lambda (e)
                                           (unless (or (eql *on-error* :debug)
                                                       (typep e 'check-failure))
                                             (when (eql *on-error* :backtrace)
                                               (trivial-backtrace:print-backtrace-to-stream
                                                *test-dribble*))
                                             (abort-test e)
                                             (return-from run-it result-list)))))
                     (restart-case
                         (handler-case
                             (let ((*readtable* (copy-readtable))
                                   (*package* (runtime-package test)))
                               (when *print-names*
                                 (format *test-dribble* "~%~ARunning test ~A " *test-dribble-indent* (name test)))
                               (if (collect-profiling-info test)
                                   ;; Timing info doesn't get collected ATM, we need a portable library
                                   ;; (setf (profiling-info test) (collect-timing (test-lambda test)))
                                   (funcall (test-lambda test))
                                   (funcall (test-lambda test))))
                           (storage-condition (e)
                             ;; heap-exhausted/constrol-stack-exhausted
                             ;; handler-case unwinds the stack (unlike handler-bind)
                             (abort-test e (format nil "STORAGE-CONDITION: aborted for safety. ~S~%~A." e e))
                             (return-from run-it result-list)))
                       (retest ()
                         :report (lambda (stream)
                                   (format stream "~@<Rerun the test ~S~@:>" test))
                         (return-from run-it (run-it)))
                       (ignore ()
                         :report (lambda (stream)
                                   (format stream "~@<Signal an exceptional test failure and abort the test ~S.~@:>" test))
                         (abort-test (make-instance 'test-failure :test-case test
                                                    :reason "Failure restart."))))
                     result-list))))
        (let ((results (run-it)))
          (setf (status test) (results-status results)
                result-list (nconc result-list results)))))))
(in-package :test)
