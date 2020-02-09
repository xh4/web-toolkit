;; sbcl --quit --disable-debugger --load pipeline.lisp
;; wx86cl64 --load pipeline.lisp
;; lispworks -load pipeline.lisp

(in-package :cl-user)

(uiop:delete-directory-tree asdf::*user-cache* :validate t)

(ql:quickload :alexandria)
(ql:quickload :cxml)
(ql:quickload :drakma)
(ql:quickload :trivial-backtrace)
(ql:quickload :fiveam)
(ql:quickload :local-time)

(defpackage :pipeline
  (:use :cl :alexandria :cxml :drakma)
  (:shadow :compile-system :load-system :test-system))

(in-package :pipeline)

(defparameter *output-stream* (make-string-output-stream))
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

;; (unless *load-truename*
;;   (error "This file must be LOADed to execute this pipeline."))

(defvar *wt-home*
  (and *load-truename*
       (make-pathname :name nil :type nil
                      :defaults *load-truename*)))

(when *wt-home*
  (push *wt-home* asdf:*central-registry*))

#+(or ccl lispworks)
(setf *debugger-hook*
      (lambda (error hook)
        (declare (ignore hook))
        (trivial-backtrace:print-backtrace error)
        (uiop:quit -1)))

(defvar *git-branch* (uiop:getenv "GIT_BRANCH"))
(defvar *git-commit* (uiop:getenv "GIT_COMMIT"))

(defvar *status-uri* "https://lisp-web-toolkit.com/status")

(local-time:reread-timezone-repository)

(defvar *pipeline-id* (local-time:format-timestring
                       nil
                       (local-time:now)
                       :format '((:year 4) #\- (:month 2) #\- (:day 2) #\-
                                 (:hour 2) #\- (:min 2) #\- (:sec 2))
                       :timezone (local-time:find-timezone-by-location-name "Asia\\Shanghai")))

(defvar *report-id* 1)

(defun make-report (system operation stage &optional condition)
  (prog1
      (with-xml-output (make-string-sink)
        (with-element "report"
          (with-element "id"
            (text (format nil "~A" *report-id*)))
          (with-element "pipeline"
            (text (format nil "~A" *pipeline-id*)))
          (with-element "system"
            (text (format nil "~A" system)))
          (with-element "operation"
            (text (format nil "~A" operation)))
          (with-element "stage"
            (text (format nil "~A" stage)))
          (with-element "condition"
            (text (format nil "~A" condition)))
          (with-element "output"
            (text (format nil "~A" (output-stream-content))))
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
            (text (namestring (uiop:getcwd))))
          (with-element "time"
            (text (local-time:format-timestring
                   nil
                   (local-time:now)
                   :timezone (local-time:find-timezone-by-location-name "Asia\\Shanghai"))))))
    (incf *report-id*)
    (make-fresh-output)))

(defun report (system operation stage &optional condition)
  ;; (format t "~%~%~A ~A ~A~%~%" system operation stage)
  (let ((report (make-report system operation stage condition)))
    (drakma:http-request
     *status-uri*
     :method :post
     :content-type "application/xml"
     :content report
     :want-stream t)))

(defun system-dependencies ()
  (labels ((wt-system-p (system)
             (let ((pos (search "wt" system :test 'equal)))
               (and pos (= pos 0))))
           (system-dependencies/1 (system)
             (when (wt-system-p system)
               (when-let ((system (asdf:find-system system)))
                 (asdf:system-depends-on system))))
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
         unless (wt-system-p dp)
         collect dp))))

(defvar *systems* '(:wt.html :wt.json :wt.uri
                    :wt.http :wt.websocket))

(defun compile-system (system)
  (make-fresh-output)
  (asdf:compile-system system))

(defun load-system (system)
  (make-fresh-output)
  (asdf:load-system system))

(defun test-system (system)
  (make-fresh-output)
  (let ((fiveam:*on-failure* :debug)
        (fiveam:*on-error* :debug))
    (asdf:test-system system)))

(defun process-system (system)
  (report system :process :start)
  (handler-bind ((error (lambda (condition)
                          (declare (ignore condition))
                          (report system :process :done))))
    (tagbody
     :compile
       (report system :compile :start)
       (handler-bind ((error (lambda (condition)
                               (report system :compile :fail condition))))
         (compile-system system))
       (report system :compile :done)

     :load
       (report system :load :start)
       (handler-bind ((error (lambda (condition)
                               (report system :load :fail condition))))
         (load-system system))
       (report system :load :done)

     :test
       (report system :test :start)
       (handler-bind ((error (lambda (condition)
                               (report system :test :fail condition))))
         (test-system system))
       (report system :test :done)
     :done
       (report system :process :done))))

(defun process-systems ()
  (report :wt :process :start)
  (loop for system in *systems*
     for index from 0
     do (handler-bind ((error (lambda (condition)
                                (declare (ignore condition))
                                (loop for system in (subseq *systems* (1+ index))
                                   do (report system :process :discard))
                                (report :wt :process :done))))
          (process-system system)))
  (report :wt :process :done))



(let ((ds (system-dependencies)))
  (ql:quickload ds))



(progn
  (process-systems)
  (uiop:quit))
