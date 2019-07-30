(in-package :http)

(defclass handler ()
  ((function
    :initarg :function
    :initform nil
    :accessor handler-function)))

(defmacro define-handler (name super-handlers slots &rest handler-options)
  `(progn
     (defclass ,name ,super-handlers
       ,slots
       ,@handler-options)
     (if (boundp ',name)
         (setf ,name (make-instance ',name))
         (defvar ,name
           (make-instance ',name)))))

(defgeneric handle (handler thing))

(defun invoke-handler (handler thing)
  (let ((method (find-method #'handle
                             '()
                             (list (class-of handler) (find-class 'request))
                             nil)))
    (when method
      (handle handler thing))))
