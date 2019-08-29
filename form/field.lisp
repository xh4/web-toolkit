(in-package :form)

(define-component field ()
  ((form
    :initarg :form
    :initform nil
    :accessor field-form)
   (name
    :initarg :name
    :initform nil
    :accessor field-name)
   (label
    :initarg :label
    :initform nil
    :accessor field-label)
   (control
    :initarg :control
    :initform nil
    :accessor field-control)
   (value
    :initarg :value
    :initform nil
    :accessor field-value)))

(defmethod shared-initialize ((field field) slot-names &rest initargs)
  (call-next-method)
  (let* ((label-class (field-label-class field))
         (label-initarg-names (field-label-initialize-argument-names field))
         (label-initargs (loop for (argname argform) on initargs by 'cddr
                            when (member argname label-initarg-names)
                            append (list argname argform))))
    (let ((label (apply 'make-instance label-class label-initargs)))
      (setf (slot-value field 'label) label)
      (let ((label-value (getf initargs :label)))
        (when label-value
          (setf (field-label field) label-value)))))
  (let* ((control-class (field-control-class field))
         (control-initarg-names (field-control-initialize-argument-names field))
         (control-initargs (loop for (argname argform) on initargs by 'cddr
                              when (member argname control-initarg-names)
                              append (list argname argform))))
    (let ((control (apply 'make-instance control-class control-initargs)))
      (setf (slot-value field 'control) control))))

(defmethod initialize-instance :after ((field field) &key)
  (with-slots (name label control) field
    (setf (label-for label) (com:id control))
    (unless (label-text label)
      (setf (label-text label) (format nil "~A" name)))))

(defmethod (setf field-value) (value (field field))
  (setf (slot-value field 'value) value)
  (setf (control-value (field-control field)) value))

(defmethod (setf field-label) (value (field field))
  (error "Don't know how to set label to ~A" value))

(defmethod (setf field-label) ((text string) (field field))
  (let ((label (field-label field)))
    (when label
      (setf (label-text label) text))))

(defmethod (setf field-label) ((label label) (field field))
  (setf (slot-value 'field 'label) label))

(defun field-definition (field)
  (let ((form (field-form field)))
    (form-field-definition form (field-name field))))

(defgeneric field-label-class (field))

(defgeneric field-label-initialize-argument-names (field))

(defgeneric field-process-label-initialize-arguments (field arguments))

(defmethod field-label-class ((field field)) 'label)

(defmethod field-label-initialize-argument-names (field) '())

(defgeneric field-control-class (field))

(defgeneric field-control-initialize-argument-names (field))

(defgeneric field-process-control-initialize-arguments (field arguments))

(define-render field (label control)
  (html:div :class "field"
            (render label)
            (render control)))
