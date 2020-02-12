(in-package :http)

(defgeneric header-fields (object))

(defgeneric (setf header-fields) (value object))

(defclass header ()
  ((fields
    :initarg :fields
    :initform '()
    :type list
    :reader header-fields)))

(defmethod (setf header-fields) (value (header header))
  (setf (slot-value header 'fields) value))

(defun process-header-objects (header objects)
  (if objects
      (let ((object (first objects)))
        (typecase object
          (header-field
           (appendf (header-fields header)
                    (list object))
           (process-header-objects header (rest objects)))
          ((or string keyword)
           (when-let ((value (second objects)))
             (appendf (header-fields header)
                      (list (header-field object value))))
           (process-header-objects header (cddr objects)))
          (t (error "Unable to use ~A as a name of header field" object))))
      header))

(defmacro header (&rest forms)
  (with-gensyms (header objects)
    `(let ((,header (make-instance 'header)))
       (let ((,objects (flatten (list ,@forms))))
         (process-header-objects ,header ,objects)))))

(defgeneric find-header-field (name header)
  (:method (name (header header))
    (find name (header-fields header)
          :test (lambda (name header-field)
                  (header-field-name-match-p header-field
                                             name)))))

(defgeneric set-header-field (header header-field)
  (:method ((header header) (header-field header-field))
    (let ((name (header-field-name header-field))
          (value (header-field-value header-field)))
      (if-let ((current-header-field (find-header-field name header)))
        (setf (header-field-value current-header-field) value)
        (appendf (header-fields header)
                 (list (make-instance 'header-field
                                      :name (header-field-name header-field)
                                      :value (header-field-value header-field))))))))

(defun read-header (stream)
  (loop for header-field = (read-header-field stream)
     while header-field
     collect header-field into fields
     finally (let ((header (make-instance 'header :fields fields)))
               (return header))))

(defgeneric write-header (stream header)
  (:method (stream (header header))
    (loop for header-field in (header-fields header)
       for size = (write-header-field stream header-field)
       sum size into header-size
       finally
         (write-sequence +crlf+ stream)
         (return (+ header-size (length +crlf+))))))
