(in-package :websocket)

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

(defun octets-to-string (vector &key (encoding :utf-8))
  (check-type vector vector)
  (if (emptyp vector)
      ""
      (progn
        (when (and (typep vector 'vector)
                   (not (typep vector '(vector (unsigned-byte 8)))))
          (setf vector (coerce vector '(vector (unsigned-byte 8)))))
        (babel:octets-to-string vector :encoding encoding))))

(defun string-to-octets (string &key (encoding :utf-8))
  (babel:string-to-octets string :encoding encoding))

(defun function-lambda-list (function)
  (etypecase function
    (cl-cont::funcallable/cc
     (rest (utility:function-lambda-list (cl-cont::f/cc-function function))))
    ((or list symbol function) (utility:function-lambda-list function))))
