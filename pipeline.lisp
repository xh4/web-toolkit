(in-package :cl-user)

(ql:quickload :alexandria)
(ql:quickload :cxml)
(ql:quickload :drakma)
(ql:quickload :trivial-backtrace)

(defpackage :pipeline
  (:use :cl :alexandria :cxml :drakma)
  (:shadow :compile-system :load-system :test-system))

(in-package :pipeline)

(defparameter *output-stream* nil)
(defparameter *original-standard-output* *standard-output*)
(defparameter *original-error-output* *error-output*)

(defun make-fresh-output ()
  (when *output-stream* (close *output-stream*))
  (setf *standard-output* *original-standard-output*
        *error-output* *original-error-output*
        *output-stream* (make-string-output-stream)
        *standard-output* (make-broadcast-stream
                           *standard-output*
                           *output-stream*)
        *error-output* (make-broadcast-stream
                        *error-output*
                        *output-stream*)))

(defun output-stream-content ()
  (get-output-stream-string *output-stream*))

(unless *load-truename*
  (error "This file must be LOADed to execute this pipeline."))

(defvar *wt-home*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(push *wt-home* asdf:*central-registry*)

#+ccl
(setf *debugger-hook*
      (lambda (error hook)
        (declare (ignore hook))
        (trivial-backtrace:print-backtrace error)
        (quit -1)))

(defvar *git-branch* (uiop:getenv "GIT_BRANCH"))
(defvar *git-commit* (uiop:getenv "GIT_COMMIT"))

(defun make-report (system operation)
  (with-xml-output (make-string-sink)
    (with-element "pipeline-event"
      (with-element "system"
        (text (format nil "~A" system)))
      (with-element "operation"
        (text (format nil "~A" operation)))
      (with-element "git-branch"
        (text *git-branch*))
      (with-element "git-commit"
        (text *git-commit*))
      (with-element "lisp-implementation-type"
        (text (lisp-implementation-type)))
      (with-element "lisp-implementation-version"
        (text (lisp-implementation-version)))
      (with-element "machine-instance"
        (text (machine-instance)))
      (with-element "machine-type"
        (text (machine-type)))
      (with-element "machine-version"
        (text (machine-version)))
      (with-element "software-type"
        (text (software-type)))
      (with-element "software-version"
        (text (software-version)))
      (with-element "user-homedir-pathname"
        (text (namestring (user-homedir-pathname))))
      (with-element "quicklisp-client-version"
        (text (ql:client-version)))
      (with-element "quicklisp-dist-version"
        (text (ql:dist-version "quicklisp")))
      (with-element "uiop-implementation-identifier"
        (text (uiop:implementation-identifier)))
      (with-element "current-working-directory"
        (text (namestring (uiop:getcwd)))))))

(defun system-dependencies ()
  (labels ((system-dependencies/1 (system)
             (asdf:system-depends-on (asdf:find-system system)))
           (system-dependencies/all (system)
             (let ((dependencies '()))
               (loop for dp in (system-dependencies/1 system)
                  do
                    (when (listp dp)
                      (if (eq (first dp) :version)
                          (setf dp (second dp))
                          (setf dp nil)))
                    (when dp
                      (appendf dependencies (list dp))
                      (appendf dependencies (system-dependencies/all dp))))
               (remove-duplicates dependencies :test 'equal))))
    (let ((dependencies (system-dependencies/all "wt/test")))
      (loop for dp in dependencies
         unless (let ((pos (search "wt." dp :test 'equal)))
                  (and pos (= pos 0)))
         collect dp))))

(defvar *systems* '(:wt.uri :wt.html :wt.json
                    :wt.http :wt.websocket
                    :wt.component :wt.form))

(defun compile-system (system)
  (make-fresh-output)
  (asdf:compile-system system))

(defun report-compile-starting (system)
  )

(defun report-compile-condition (system condition)
  )

(defun report-compile-completion (system)
  (format t "Compile done: ~A~%" system))

(defun load-system (system)
  (make-fresh-output)
  (asdf:load-system system))

(defun report-load-starting (system)
  )

(defun report-load-condition (system condition)
  )

(defun report-load-completion (system)
  (format t "Load done: ~A~%" system))

(defun report-load-abortion (system)
  (format t "Load abort: ~A~%" system))

(defun test-system (system)
  (make-fresh-output)
  (asdf:test-system system))

(defun report-test-starting (system)
  )

(defun report-test-condition (system condition)
  )

(defun report-test-completion (system)
  (format t "Test done: ~A~%" system))

(defun report-test-abortion (system)
  (format t "Test abort: ~A~%" system))

(defun process-system (system)
  (let (start-load start-test)
    (tagbody
     :compile
       (report-compile-starting system)
       (handler-bind ((error (lambda (condition)
                               (report-compile-condition system condition)
                               (go :done))))
         (compile-system system))
       (report-compile-completion system)

     :load
       (setf start-load t)
       (handler-bind ((error (lambda (condition)
                               (report-load-condition system condition)
                               (go :done))))
         (load-system system))
       (report-load-completion system)

     :test
       (setf start-test t)
       (handler-bind ((error (lambda (condition)
                               (report-test-condition system condition))))
         (test-system system))
       (report-test-completion system)

     :done
       (unless start-load
         (report-load-abortion system))
       (unless start-test
         (report-test-abortion system)))))

(defun process-systems ()
  (loop for system in *systems*
     do (process-system system)))



(let ((ds (system-dependencies)))
  (ql:quickload ds))

(process-systems)

#+(or ccl lispworks)
(quit)
