(in-package :component)

(defclass reflective-object ()
  ((dependency
    :initarg :dependency
    :initform nil
    :accessor object-dependency)
   (propagation
    :initarg :propagation
    :initform nil
    :accessor object-propagation)))

(defclass reflective-class (standard-class reflective-object) ())

(defclass reflective-method (closer-mop:standard-method reflective-object) ())

(defmethod validate-superclass
    ((class reflective-class) (super-class standard-class)) t)

(defun object-dependency/0 (object)
  (mapcar #'trivial-garbage:weak-pointer-value (object-dependency object)))

(defun object-propagation/0 (object)
  (mapcar #'trivial-garbage:weak-pointer-value (object-propagation object)))

(defun propagation-list (object)
  (let ((propagation-list `(,object)))
    (labels ((add-propagation (pointer)
               (when-let ((object (trivial-garbage:weak-pointer-value pointer)))
                 (pushnew object propagation-list)
                 (loop for pointer in (object-propagation object)
                    do (add-propagation pointer)))))
      (loop for pointer in (object-propagation object)
         do (add-propagation pointer)))
    (reverse propagation-list)))

(defun detect-cycle (object dependency)
  (labels ((walk (pointer path)
             (when-let ((o (trivial-garbage:weak-pointer-value pointer)))
               (push o path)
               (if (eq o object)
                   (error "Cycle object dependency from ~A, path: ~A" object (reverse path))
                   (loop for pointer in (object-dependency o)
                      do (walk pointer path))))))
    (loop for pointer in dependency
       do (walk pointer `(,(trivial-garbage:weak-pointer-value pointer))))))

(defun add-dependency (object-1 object-2)
  (when (and object-1 object-2)
    (let ((new-dependency (cons
                           (trivial-garbage:make-weak-pointer object-2)
                           (object-dependency object-1))))
      (detect-cycle object-1 new-dependency))
    (pushnew (trivial-garbage:make-weak-pointer object-2) (object-dependency object-1))
    (pushnew (trivial-garbage:make-weak-pointer object-1) (object-propagation object-2))))

(defun remove-dependency (object-1 object-2)
  (when (and object-1 object-2)
    (setf (object-dependency object-1)
          (remove object-2 (object-dependency object-1)
                  :key #'trivial-garbage:weak-pointer-value))
    (setf (object-propagation object-2)
          (remove object-1 (object-propagation object-2)
                  :key #'trivial-garbage:weak-pointer-value))))

(defun set-dependency (object dependency)
  (let ((current-dependency (object-dependency/0 object)))
    (let ((to-add (set-difference dependency current-dependency))
          (to-remove (set-difference current-dependency dependency)))
      (loop for o in to-add
         do (add-dependency object o))
      (loop for o in to-remove
         do (remove-dependency object o)))))

(defgeneric update (source update)
  (:method ((source reflective-object) update)
    (loop for object in (propagation-list source)
       do (reflect object source update))))

(defgeneric reflect (object source update)
  (:method ((object reflective-object) (source reflective-object) update)))

(defmethod shared-initialize :after ((class reflective-class) slot-names &rest initargs &key &allow-other-keys)
  (update class t))

(defmethod shared-initialize :after ((object reflective-object) slot-names &rest initargs &key &allow-other-keys)
  (let ((class (class-of object)))
    (when (typep class 'reflective-class)
      (add-dependency object class))))

(defmethod shared-initialize :after ((method reflective-method) slot-names &rest initargs &key &allow-other-keys)
  ;; TODO: update the corresponding classes
  (update method t))
