(in-package :component)

(defclass component-class (standard-class)
  ((tag-options
    :initarg :tag-options
    :initform nil
    :accessor component-class-tag-options)
   (class-options
    :initarg :class-options
     :initform nil
     :accessor component-class-class-options)))


(defun direct-slot-definition->initargs (slot-definition)
  (list :name (closer-mop:slot-definition-name slot-definition)
        :readers (closer-mop:slot-definition-readers slot-definition)
        :writers (closer-mop:slot-definition-writers slot-definition)
        :initargs (closer-mop:slot-definition-initargs slot-definition)
        :initform (closer-mop:slot-definition-initform slot-definition)
        :allocation (closer-mop:slot-definition-allocation slot-definition)
	:type (closer-mop:slot-definition-type slot-definition)
	:documentation (documentation slot-definition t)
        :initfunction (closer-mop:slot-definition-initfunction slot-definition)))

(defun add-slot-to-class (class slot-name &key (initform nil)
                                       accessors readers writers
                                       initargs
                                       (initfunction (constantly nil)))
  (let ((new-slots (list (list :name slot-name
                               :readers (union accessors readers)
                               :writers (union writers
                                               (mapcar #'(lambda (x)
                                                           (list 'setf
                                                                 x))
                                                       accessors)
                                               :test #'equal)
                               :initform initform
                               :initargs initargs
                               :initfunction initfunction))))
    (dolist (current-slot (closer-mop:class-direct-slots class))
      (push (direct-slot-definition->initargs current-slot)
            new-slots))
    (closer-mop:ensure-class
     (class-name class)
     :metaclass (class-of class)
     :direct-superclasses (closer-mop:class-direct-superclasses class)
     :direct-slots new-slots
     :direct-default-initargs (closer-mop:class-direct-default-initargs class))))
