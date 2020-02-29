(in-package :component)

(defclass variable ()
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
    :reader variable-value)
   (dependency
    :initarg :dependency
    :initform nil
    :accessor variable-dependency)
   (propagation
    :initarg :propagation
    :initform nil
    :accessor variable-propagation)))

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
    (format t "Set form of ~A from ~A to ~A~%" variable current-form target-form)
    (setf (slot-value variable 'form) target-form)
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (setf (slot-value variable 'form) current-form
                                  (slot-value variable 'value) current-value))))
      (loop for v in (propagation-list variable)
         do (reinitialize v)))))

(defvar *dependency* nil)

(defun reinitialize (variable)
  (setf (slot-value variable 'value)
        (eval `(let ((*variable* ,variable)
                     (*dependency* nil))
                 (prog1
                     ,(variable-form variable)
                   (set-dependency ,variable *dependency*))))))

(defun propagation-list (variable)
  (let ((propagation-list `(,variable)))
    (labels ((add-propagation (v)
               (pushnew v propagation-list)
               (loop for v in (variable-propagation v)
                  do (add-propagation v))))
      (loop for v in (variable-propagation variable)
         do (add-propagation v)))
    (reverse propagation-list)))

(defun detect-cycle (variable dependency)
  (labels ((walk (v path)
             (push v path)
             (if (eq v variable)
                 (error "Cycle variable dependency from ~A, path: ~A" variable (reverse path))
                 (loop for v in (variable-dependency v)
                    do (walk v path)))))
    (loop for v in dependency
         do (walk v `(,v)))))

(defun add-dependency (variable-1 variable-2)
  (when (and variable-1 variable-2)
    (let ((new-dependency (cons variable-2 (variable-dependency variable-1))))
      (detect-cycle variable-1 new-dependency))
    (pushnew variable-2 (variable-dependency variable-1))
    (pushnew variable-1 (variable-propagation variable-2))))

(defun remove-dependency (variable-1 variable-2)
  (when (and variable-1 variable-2)
    (setf (variable-dependency variable-1)
          (remove variable-2 (variable-dependency variable-1)))
    (setf (variable-propagation variable-2)
          (remove variable-1 (variable-propagation variable-2)))))

(defun set-dependency (variable dependency)
  (let ((current-dependency (variable-dependency variable)))
    (let ((to-add (set-difference dependency current-dependency))
          (to-remove (set-difference current-dependency dependency)))
      (loop for v in to-add
         do (add-dependency variable v))
      (loop for v in to-remove
         do (remove-dependency variable v)))))

(defmethod print-object ((variable variable) stream)
  (print-unreadable-object (variable stream :type t :identity t)
    (format stream "~A (~A)"
            (variable-name variable)
            (variable-value variable))))

(defvar *variable* nil)

(defmacro define-variable (name form)
  (let ((vname (intern (format nil "V/~A" name))))
    `(if (boundp ',vname)
         (let ((variable (variable ',name)))
           (if (equal ',form (variable-form variable))
               ',name
               (setf (variable-form ,vname) ',form)))
         (progn
           (defvar ,vname (make-instance 'variable
                                         :name ',name
                                         :form ',form))
           (handler-case
                (setf (variable-form ,vname) ',form)
             (error (e)
               (declare (ignore e))
               (makunbound ',vname)))
           (define-symbol-macro ,name (prog1
                                          (variable-value ,vname)
                                        (when *variable*
                                          (push ,vname *dependency*))))))))
