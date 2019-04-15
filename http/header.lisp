(in-package :http)


(defgeneric header-fields (header))

(defgeneric (setf header-fields) (value header))

(defgeneric header-field (header name))

(defgeneric (setf header-field) (value header name))


(defgeneric field-name (field))

(defgeneric (setf field-name) (name field))

(defgeneric field-value (field))

(defgeneric (setf field-value) (value field))

(defgeneric field-name-match-p (field name))


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
     when (field-name-match-p field name)
     return field))

(defmethod (setf header-field) (value (header header) name)
  (let (found)
    (loop for field in (header-fields header)
       when (field-name-match-p field name)
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
    :reader field-name)
   (value
    :initarg :value
    :reader field-value)))

(defmethod print-object ((field header-field) stream)
  (print-unreadable-object (field stream :type t)
    (format stream "~A: ~A" (field-name field) (field-value field))))

(defmethod initialize-instance :after ((field header-field) &key)
  (setf (field-name field) (slot-value field 'name)
        (field-value field) (slot-value field 'value)))

(defmethod (setf field-name) (name field)
  (let ((name (typecase name
                (string name)
                (t (header-case (format nil "~A" name))))))
    (setf (slot-value field 'name) name)))

(defmethod (setf field-value) (value field)
  (let ((value (typecase value
                 (string value)
                 (t (format nil "~A" value)))))
    (setf (slot-value field 'value) value)))

(defmethod field-name-match-p (field name)
  (let ((name (typecase name
                (string name)
                (t (format nil "~A" name)))))
    (string-equal name (field-name field))))
