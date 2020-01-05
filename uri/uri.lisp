(in-package :uri)

(defmacro uri (&body forms)
  (with-gensyms (values)
    `(let ((,values (list ,@forms)))
       (typecase (car ,values)
         (null nil)
         (string (apply 'parse-uri (car ,values) (rest ,values)))
         (keyword (apply 'make-instance 'uri ,values))))))
