(in-package :component)

(shadow :variable)

(defclass variable ()
  ((name
    :initarg :name
    :initform nil
    :reader variable-name)
   (form
    :initarg :form
    :initform nil
    :reader variable-form)
   (value
    :initarg :value
    :initform nil
    :accessor variable-value)
   (dependency
    :initarg :dependency
    :initform nil
    :accessor variable-dependency)
   (propagation
    :initarg :propagation
    :initform nil
    :accessor variable-propagation)))

(defvar *variable* nil)

(defun add-dependency (variable-1 variable-2)
  (when (and variable-1 variable-2)
    (pushnew variable-2 (variable-dependency variable-1))
    (pushnew variable-1 (variable-propagation variable-2))))

(defmethod print-object ((variable variable) stream)
  (print-unreadable-object (variable stream :type t :identity t)
    (format stream "~A (~A)"
            (variable-name variable)
            (variable-value variable))))

(defmacro define-variable (name form)
  (let ((vname (intern (format nil "V_~A" name))))
    `(if (boundp ',vname)
         ',name
         (progn
           (defvar ,vname (make-instance 'variable
                                         :name ',name
                                         :form ',form))
           (handler-case
                (setf (variable-value ,vname)
                      (let ((*variable* ,vname))
                        ,form))
             (error (e)
               (declare (ignore e))
               (unintern ',vname)))
           (define-symbol-macro ,name (prog1
                                          (variable-value ,vname)
                                        (add-dependency *variable* ,vname)))))))
