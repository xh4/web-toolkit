(in-package :cl-user)

(ql:quickload :cxml)
(ql:quickload :drakma)
(ql:quickload :trivial-backtrace)

(use-package :cxml)
(use-package :drakma)
(use-package :trivial-backtrace)

(defparameter *output-stream* (make-string-output-stream))

(setf *standard-output* (make-broadcast-stream
                         *standard-output*
                         *output-stream*)
      *error-output* (make-broadcast-stream
                      *error-output*
                      *output-stream*))

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
        (print-backtrace error)
        (quit -1)))

(defvar *git-branch* (uiop:getenv "GIT_BRANCH"))
(defvar *git-commit* (uiop:getenv "GIT_COMMIT"))

(with-xml-output (make-string-sink)
  (with-element "pipeline-event"
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
      (text (namestring (uiop:getcwd))))))

;; 1. Compile & Load
;; 2. Test

;; event-type: start-build | done-build | fail-build

(ql:quickload "wt")

#+(or ccl lispworks)
(quit)
