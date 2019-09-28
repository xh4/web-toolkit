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

(defmacro header (&rest forms)
  `(let ((header (make-instance 'header)))
     (labels ((handle-header-forms (forms)
                (if forms
                    (let ((object (eval (first forms))))
                      (typecase object
                        (header-field
                         (appendf (header-fields header)
                                  (list object))
                         (handle-header-forms (rest forms)))
                        ((or string keyword)
                         (if (second forms)
                             (let ((value (eval (second forms))))
                               (appendf (header-fields header)
                                        (list (header-field object value)))
                               (handle-header-forms (cddr forms)))
                             (error "Missing header field value for name ~S" object)))
                        (t (error "Unable to use ~A as a name of header field" object))))
                    header)))
       (handle-header-forms ',forms))))

(defgeneric find-header-field (object name)
  (:method ((header header) name)
    (find name (header-fields header)
          :test (lambda (name header-field)
                  (header-field-name-match-p header-field
                                             name)))))
