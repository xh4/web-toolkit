(in-package :utility)

(defclass variable (reflective-object)
  ((name
    :initarg :name
    :initform nil
    :reader variable-name)
   (form
    :initarg :form
    :initform nil
    :accessor variable-form)
   (value
    :initarg :value
    :initform nil
    :reader variable-value)))

(defmacro variable (name)
  (with-gensyms (object)
    `(let ((,object ,name))
       (typecase ,object
         (variable ,object)
         (symbol (symbol-value (intern (format nil "V/~A" ,name))))
         (t (error "Not a variable form ~A" object))))))

;; TODO: better error report
(defmethod (setf variable-form) (form (variable variable))
  (let ((current-form (slot-value variable 'form))
        (current-value (slot-value variable 'value))
        (target-form form))
    ;; (format t "Set form of ~A from ~A to ~A~%" variable current-form target-form)
    (setf (slot-value variable 'form) target-form)
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (setf (slot-value variable 'form) current-form
                                  (slot-value variable 'value) current-value))))
      (reify variable)
      (update variable t))))

(defmethod reflect ((variable variable) object update)
  (reify variable)
  (update variable t))

(defvar *dependency* nil)

(defun reify (variable)
  (setf (slot-value variable 'value)
        (eval `(let ((*variable* ,variable)
                     (*dependency* nil))
                 (prog1
                     ,(variable-form variable)
                   (set-dependency ,variable *dependency*))))))

(defmethod initialize-instance :after ((variable variable) &key)
  (reify variable))

(defmethod print-object ((variable variable) stream)
  (print-unreadable-object (variable stream :type t :identity t)
    (format stream "~A (~A)"
            (variable-name variable)
            (variable-value variable))))

(defvar *variable* nil)

(defmacro define-variable (name form)
  (let ((vname (intern (format nil "V/~A" name))))
    `(progn
       (defvar ,vname (make-instance 'variable
                                     :name ',name
                                     :form ',form))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ;; TODO: check if `name` is already global variable
         (define-symbol-macro ,name (prog1
                                        (variable-value ,vname)
                                      (when *variable*
                                        (push ,vname *dependency*)))))
       (eval-when (:load-toplevel :execute)
         (unless (equal ',form (variable-form ,vname))
           (setf (variable-form ,vname) ',form))))))
