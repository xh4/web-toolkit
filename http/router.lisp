(in-package :http)

(define-handler router (handler)
  ())

(defmacro define-router (name)
  `(if (boundp ',name)
       (progn
         (setf ,name (make-instance 'router))
         ,name)
       (defvar ,name
         (make-instance 'router))))

(defmacro router ()
  `(make-instance 'router))

(defmethod handle ((router router) (request request))
  )
