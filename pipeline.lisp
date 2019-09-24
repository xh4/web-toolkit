(in-package :cl-user)

;; sbcl --quit --disable-debugger --load website/pipeline.lisp
;; wx86cl64 --load website/pipeline.lisp

(ql:quickload :cxml)
(ql:quickload :drakma)

(defparameter *output-stream*
  (open "c:/users/xh/output.txt"
        :direction :output
        :if-exists :supersede
        :external-format :utf-8))

(setf *standard-output* (make-broadcast-stream
                         *standard-output*
                         *output-stream*)
      *error-output* (make-broadcast-stream
                      *error-output*
                      *output-stream*))

(format t "Hello, world")
