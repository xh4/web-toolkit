(in-package :website)

(defvar *acceptor* nil)

(defun start-server ()
  (when (not *acceptor*)
    (handler-case
        (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port 8080)))
          (setf *acceptor* acceptor)
          (hunchentoot:start *acceptor*)
          *acceptor*)
      (error (e)
        (format t "~A~%" e)
        (setf *acceptor* nil)))))

(defun stop-server ()
  (when *acceptor*
    (handler-case
        (hunchentoot:stop *acceptor*)
      (error (e)
        (format t "~A~%" e)))
    (setf *acceptor* nil)
    t))
