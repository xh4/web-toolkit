(in-package :wt)

(defclass web-page (web-handler)
  ())

(defmacro define-web-page (name &rest body)
  (let ((level-0 `(lambda (req) ,@body)))
    `(defclass ,name (web-page)
       ((level-0 :initform ,level-0)))))

(define-web-page index-page ()

    (&bind
     ()
     &validate
     (fjewoifjewf
      ))

  (let ((text "Hello, world"))
    `(200 (:content-type "text/plain") (,text))))

(define-condition yield ()
  ())

(let* ((fn1 (lambda ()
              (format t "> 1~%")
              (handler-case
                  (format t "~A~%" (restart-case
                                       (error 'yield)
                                     (use-value (continuation)
                                       (funcall continuation))))
                (error (e)
                  (format t "Catched ~A~%" e)))

              (format t "< 1~%")
              1))
       (fn2 (lambda ()
              (format t "> 2~%")
              (format t "~A~%" (restart-case
                                   (error 'yield)
                                 (use-value (continuation)
                                   (funcall continuation))))
              (format t "< 2~%")
              2))
       (fn3 (lambda ()
              (format t "> 3~%")
              (error "Kekeke")
              (format t "< 3~%")
              3))
       (fns (list fn1 fn2 fn3))
       (continuations (cdr fns)))
  (handler-bind ((yield (lambda (e)
                          (let ((continuation (car continuations)))
                            (setf continuations (cdr continuations))
                            (use-value continuation)))))
    (funcall fn1)))
