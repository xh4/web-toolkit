(in-package :cl-user)

;; sbcl --quit --disable-debugger --load pipeline.lisp
;; wx86cl64 --load pipeline.lisp

(ql:quickload :cxml)
(ql:quickload :drakma)

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

(asdf:load-system "wt")
