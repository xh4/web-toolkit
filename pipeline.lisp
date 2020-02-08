;; sbcl --quit --disable-debugger --load pipeline.lisp
;; wx86cl64 --load pipeline.lisp
;; lispworks -load pipeline.lisp

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

;; (unless *load-truename*
;;   (error "This file must be LOADed to execute this pipeline."))

(eval-when (:load-toplevel)
  (defvar *wt-home*
    (make-pathname :name nil :type nil
                   :defaults *load-truename*))
  (push *wt-home* asdf:*central-registry*))

#+ccl
(eval-when (:load-toplevel)
  (setf *debugger-hook*
        (lambda (error hook)
          (declare (ignore hook))
          (trivial-backtrace:print-backtrace error)
          (quit -1))))

(defvar *git-branch* (uiop:getenv "GIT_BRANCH"))
(defvar *git-commit* (uiop:getenv "GIT_COMMIT"))

(defvar *status-uri* "http://127.0.0.1:7000/status")

(defun make-report (system operation stage &optional condition)
  (with-xml-output (make-string-sink)
    (with-element "pipeline-event"
      (with-element "system"
        (text (format nil "~A" system)))
      (with-element "operation"
        (text (format nil "~A" operation)))
      (with-element "stage"
        (text (format nil "~A" stage)))
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

(defun report (system operation stage &optional condition)
  (format t "~A ~A ~A ~A~%" system operation stage condition)
  ;; (make-report system operation stage condition)

  )

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
                    ;; :wt.http :wt.websocket
                    ))

(defun compile-system (system)
  (make-fresh-output)
  (asdf:compile-system system))

(defun load-system (system)
  (make-fresh-output)
  (asdf:load-system system))

(defun test-system (system)
  (make-fresh-output)
  (asdf:test-system system))

(defun process-system (system)
  (let (start-load start-test)
    (tagbody
     :compile
       (report system :compile :start)
       (handler-bind ((error (lambda (condition)
                               (report system :compile :fail condition)
                               (go :all-done))))
         (compile-system system))
       (report system :compile :done)

     :load
       (setf start-load t)
       (report system :load :start)
       (handler-bind ((error (lambda (condition)
                               (report system :load :fail condition)
                               (go :all-done))))
         (load-system system))
       (report system :load :done)

     :test
       (setf start-test t)
       (report system :test :start)
       (handler-bind ((error (lambda (condition)
                               (report system :test :fail condition))))
         (test-system system))
       (report system :test :done)

     :all-done
       (unless start-load
         (report system :load :discard))
       (unless start-test
         (report system :test :discard)))))

(defun process-systems ()
  (report :wt :process :start)
  (loop for system in *systems*
     do (process-system system))
  (report :wt :process :done))



(let ((ds (system-dependencies)))
  (ql:quickload ds))


(eval-when (:load-toplevel)
  (process-systems)
  #+(or ccl lispworks)
  (quit 0))
