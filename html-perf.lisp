(in-package :wt.html)

(ql:quickload :cl-who)
(ql:quickload :spinneret)
(ql:quickload :cl-markup)

(defparameter *test-html*
  `(:div :class "container"
         ,@(loop for i upto 999
              collect `(:div :class "content"
                             ,i))))

(defun test-wt-html-perf ()
  (time (progn (html-string *test-html*) nil)))

(defmacro generate-test-spinneret-html-perf-function ()
  `(defun test-spinneret-html-perf ()
     (time
      (progn
        (spinneret:with-html-string
          ,*test-html*)
        nil))))

;; Failed to generate 10000 elements on SBCL.

;; (generate-test-spinneret-html-perf-function)

;; (test-spinneret-html-perf)

(let ((*enter-element-hook*
       (lambda (tag attributes)
         (format t "> ~A~%" tag)))
      (*leave-element-hook*
       (lambda (tag)
         (format t "< ~A~%" tag)))
      (*text-content-hook*
       (lambda (form)
         (format t "~S~%" form))))
  (traverse-html '((:!doctype)
                   (:html
                    (:head
                     (:meta :charset "utf-8")
                     (:title "Foo"))
                    (:body
                     )))))
