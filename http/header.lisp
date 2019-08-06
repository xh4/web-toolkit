(in-package :http)

(defgeneric header-fields (object))

(defgeneric (setf header-fields) (value object))

(defgeneric header-field (object name))

(defgeneric (setf header-field) (value object name))


(defgeneric header-field-name (header-field))

(defgeneric (setf header-field-name) (name header-field))

(defgeneric header-field-value (header-field))

(defgeneric (setf header-field-value) (value header-field))

(defgeneric header-field-name-match-p (header-field name))


(defclass header ()
  ((fields
    :initarg :fields
    :initform '()
    :type list
    :reader header-fields)))

(defmethod print-object ((header header) stream)
  (print-unreadable-object (header stream :type t)
    (when (header-fields header)
      (loop for field in (header-fields header)
         do (format stream "~%  ~A" field)))))

(defmethod (setf header-fields) (value (header header))
  (setf (slot-value header 'fields) value))

(defmethod header-field ((header header) name)
  (loop for field in (header-fields header)
     when (header-field-name-match-p field name)
     return field))

(defmethod (setf header-field) (value (header header) name)
  (let (found)
    (loop for field in (header-fields header)
       when (header-field-name-match-p field name)
       do (progn
            (setf (field-value field) value
                  found t)
            (return field))
       finally
         (when (not found)
           (let ((field (make-instance 'header-field :name name :value value)))
             (appendf (header-fields header) (list field))
             (return field))))))


(defclass header-field ()
  ((name
    :initarg :name
    :reader header-field-name)
   (value
    :initarg :value
    :reader header-field-value)))

(defmethod print-object ((field header-field) stream)
  (print-unreadable-object (field stream :type t)
    (format stream "~A: ~A" (header-field-name field) (header-field-value field))))

(defmethod initialize-instance :after ((field header-field) &key)
  (setf (header-field-name field) (slot-value field 'name)
        (header-field-value field) (slot-value field 'value)))

(defmethod (setf header-field-name) (name field)
  (let ((name (typecase name
                (string name)
                (t (header-case (format nil "~A" name))))))
    (setf (slot-value field 'name) name)))

(defmethod header-field-name ((field null))
  nil)

(defmethod (setf header-field-value) (value field)
  (let ((value (typecase value
                 (string value)
                 (t (format nil "~A" value)))))
    (setf (slot-value field 'value) value)))

(defmethod header-field-value ((field null))
  nil)

(defmethod header-field-name-match-p (header-field name)
  (let ((name (typecase name
                (string name)
                (t (format nil "~A" name)))))
    (string-equal name (header-field-name header-field))))
