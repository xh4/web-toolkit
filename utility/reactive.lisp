(in-package :utility)

(defclass reactive-object ()
  ((dependency
    :initarg :dependency
    :initform nil
    :accessor object-dependency)
   (propagation
    :initarg :propagation
    :initform nil
    :accessor object-propagation)))

(defclass reactive-class (standard-class reactive-object) ())

(defclass reactive-method (closer-mop:standard-method reactive-object) ())

(defmethod closer-mop:validate-superclass
    ((class reactive-class) (super-class standard-class)) t)

(defun reactive-object-p (object)
  (typep object 'reactive-object))

(defun object-dependency/0 (object)
  (mapcar #'trivial-garbage:weak-pointer-value (object-dependency object)))

(defun object-propagation/0 (object)
  (mapcar #'trivial-garbage:weak-pointer-value (object-propagation object)))

(defun propagation-list (object)
  (let ((propagation-list '()))
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
  (:method ((source reactive-object) update)
    ;; (reflect object object update)
    (loop for pointer in (object-propagation source)
       for object = (trivial-garbage:weak-pointer-value pointer)
       when object
       do (reflect object source update))))

(defgeneric reflect (object source update)
  (:method ((object reactive-object) (source reactive-object) update)))

(defmethod shared-initialize :after ((class reactive-class) slot-names &rest initargs &key &allow-other-keys)
  (labels ((update-class (class)
             (update class t)
             (mapcar #'update-class
                     (closer-mop:class-direct-subclasses class))))
    (update-class class)))

(defmethod shared-initialize :around ((object reactive-object) slot-names &rest initargs &key &allow-other-keys)
  (let ((object (call-next-method)))
    (trivial-garbage:finalize
     object
     (lambda ()
       (loop for pointer in (object-dependency object)
          for o = (trivial-garbage:weak-pointer-value pointer)
          when o
          do (setf (object-propagation o)
                   (remove object (object-propagation o)
                           :key #'trivial-garbage:weak-pointer-value)))
       (loop for pointer in (object-propagation object)
          for o = (trivial-garbage:weak-pointer-value pointer)
          when o
          do (setf (object-dependency o)
                   (remove object (object-dependency o)
                           :key #'trivial-garbage:weak-pointer-value)))
       object))
    object))

(defmethod shared-initialize :after ((object reactive-object) slot-names &rest initargs &key &allow-other-keys)
  (let ((class (class-of object)))
    (when (typep class 'reactive-class)
      (add-dependency object class))))

(defmethod shared-initialize :after ((method reactive-method) slot-names &rest initargs &key &allow-other-keys)
  (loop for class in (closer-mop:method-specializers method)
     when (typep class 'reactive-class)
     do (update class t)))
