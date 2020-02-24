(in-package :utility)

(defmacro replace-class-option (name key &rest values)
  (with-gensyms (pos/s)
    `(if-let ((,pos/s (position ,key ,name :key 'first)))
       (setf (nth ,pos/s ,name) (list ,key ,@values))
       (appendf ,name (list (list ,key ,@values))))))

(defmacro rewrite-class-option (name key &rest values)
  (let ((option (cons key values)))
    (with-gensyms (pos/s)
      `(if-let ((,pos/s (position ,key ,name :key 'first)))
         (setf (nth ,pos/s ,name) ',option)
         (appendf ,name (list ',option))))))
