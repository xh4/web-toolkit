(in-package :http)

(defgeneric header-fields (object))

(defgeneric (setf header-fields) (value object))

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

(defgeneric find-header-field (object name)
  (:method ((header header) name)
    (find name (header-fields header)
          :test (lambda (name header-field)
                  (header-field-name-match-p header-field
                                             name)))))
