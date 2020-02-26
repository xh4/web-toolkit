(in-package :http)

(defclass form ()
  ((fields
    :initarg :fields
    :initform nil
    :accessor form-fields)))

(defclass form-field ()
  ((name
    :initarg :name
    :initform nil
    :accessor form-field-name)
   (value
    :initarg :value
    :initform nil
    :accessor form-field-value)))

(defmacro form (&rest forms)
  (with-gensyms (form objects)
    `(let ((,form (make-instance 'form)))
       (let ((,objects (flatten (list ,@forms))))
         (process-form-objects ,form ,objects)))))

(defmacro form-field (name value)
  `(make-instance 'form-field :name ,name :value ,value))

(defun process-form-objects (form objects)
  (if objects
      (let ((object (first objects)))
        (typecase object
          (form-field
           (set-form-field form object)
           (process-form-objects form (rest objects)))
          ((or string keyword)
           (if-let ((value (second objects)))
             (set-form-field form (form-field object value))
             (error "Missing form field value for name ~S" object))
           (process-form-objects form (cddr objects)))
          (t (error "Unable to use ~A as a name of form field" object))))
      form))

(defgeneric find-form-field (name form)
  (:method (name (form form))
    (find name (form-fields form)
          :test 'equal)))

(defgeneric set-form-field (form form-field)
  (:method ((form form) (form-field form-field))
    (let ((name (form-field-name form-field))
          (value (form-field-value form-field)))
      (if-let ((current-form-field (find-form-field name form)))
        (setf (form-field-value current-form-field) value)
        (appendf (form-fields form)
                 (list (make-instance 'form-field
                                      :name (form-field-name form-field)
                                      :value (form-field-value form-field))))))))

;; "application/x-www-form-urlencoded"
;; "multipart/form-data"
(defgeneric form-content-type (form)
  (:method ((form form))
    "application/x-www-form-urlencoded"))

(defgeneric serialize-form (form)
  (:method ((form form))
    (let ((query (loop for field in (form-fields form)
                    collect (cons (form-field-name field)
                                  (form-field-value field)))))
      (when query
        (let ((data (subseq (uri-string :query query) 1)))
          (babel:string-to-octets data))))))


(defmacro skip-form-field ()
  `(invoke-restart 'skip-form-field))



(defmacro read-form-field-into-vector ()
  `(invoke-restart 'read-form-field-into-vector))



(defmacro read-form-field-into-stream (stream)
  `(invoke-restart 'read-form-field-into-stream ,stream))



(defmacro read-form-field-into-file (pathname)
  `(invoke-restart 'read-form-field-into-file ,pathname))



(defmacro read-form-field-into-temporary-file ()
  `(invoke-restart 'read-form-field-into-temporary-file))
